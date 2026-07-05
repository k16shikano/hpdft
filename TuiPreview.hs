{-# LANGUAGE OverloadedStrings #-}

module TuiPreview (runTuiPreview) where

import PDF.Document (Document)
import PDF.Text (pdfToTextStreamDoc)
import PDF.Error (PdfWarning, renderPdfWarning)
import TuiScroll
  ( ScrollState(..)
  , initialScrollState
  , clampScrollTop
  , scrollBy
  , scrollToTop
  , scrollToEnd
  , scrollHalfPageDown
  , scrollHalfPageUp
  , searchForwardFrom
  , searchBackwardFrom
  , visibleLineRange
  , statusLineNumber
  , stringDisplayWidth
  , clipToDisplayWidth
  , padToDisplayWidth
  )

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (AsyncException(UserInterrupt), bracket, catch, finally, throwIO)
import Control.Monad (when)
import Data.Array ((!))
import Data.Char (isPrint)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import System.Console.Terminal.Size (Window(..), hSize)
import System.IO
  ( BufferMode(..)
  , Handle
  , hFlush
  , hGetBuffering
  , hGetChar
  , hGetEcho
  , hPutStr
  , hPutStrLn
  , hReady
  , hSetBuffering
  , hSetEcho
  , stderr
  , stdin
  , stdout
  )
import Text.Regex.Base.RegexLike (makeRegexM, matchAll, matchTest)
import Text.Regex.TDFA (Regex)

runTuiPreview :: FilePath -> Document -> IO ()
runTuiPreview path doc = do
  warningsRef <- newIORef []
  let run term =
        uiLoop term path doc warningsRef `catch` \e ->
          case e of
            UserInterrupt -> return ()
            _ -> throwIO e
  bracket acquireTerminal releaseTerminal run
  ws <- readIORef warningsRef
  mapM_ (hPutStrLn stderr . ("hpdft: warning: " ++) . renderPdfWarning) ws

data Terminal = Terminal
  { termStdout      :: Handle
  , termStderr      :: Handle
  , termStdin       :: Handle
  , termOldEcho     :: Bool
  , termOldBuffer   :: BufferMode
  , termRows        :: Int
  , termCols        :: Int
  , termViewportH   :: Int
  , termTextRows    :: Int
  , termViewportTop :: Int
  }

data Key
  = KChar Char
  | KUp
  | KDown
  | KPageUp
  | KPageDown
  | KEnter
  | KBackspace
  | KEsc
  | KOther

-- | Status-line mode: normal view, or entering a search pattern after @/@.
data UiMode = ModeView | ModeInput String

acquireTerminal :: IO Terminal
acquireTerminal = do
  oldEcho <- hGetEcho stdin
  oldBuf <- hGetBuffering stdin
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  (rows, cols, vh, textRows, vTop) <- queryGeometry stdout
  -- Hide the cursor and disable autowrap: a wrap on the bottom row would
  -- scroll the whole screen and drag the viewport out of place.
  hPutStr stdout "\ESC[?25l\ESC[?7l"
  hFlush stdout
  return Terminal
    { termStdout = stdout
    , termStderr = stderr
    , termStdin = stdin
    , termOldEcho = oldEcho
    , termOldBuffer = oldBuf
    , termRows = rows
    , termCols = cols
    , termViewportH = vh
    , termTextRows = textRows
    , termViewportTop = vTop
    }

releaseTerminal :: Terminal -> IO ()
releaseTerminal term = do
  clearViewport term
  hPutStr (termStdout term) "\ESC[?25h\ESC[?7h\ESC[0m"
  hPutStr (termStdout term) $ "\ESC[" ++ show (termRows term + 1) ++ ";1H"
  hFlush (termStdout term)
  hSetEcho (termStdin term) (termOldEcho term)
  hSetBuffering (termStdin term) (termOldBuffer term)

-- Viewport layout: top bar, text rows, status bar (bottom).
queryGeometry :: Handle -> IO (Int, Int, Int, Int, Int)
queryGeometry h = do
  mw <- hSize h
  let reported f dflt = case mw of
        Just w | fromIntegral (f w) > 0 -> fromIntegral (f w)
        _ -> dflt
      rows = reported height 24
      cols = reported width 80
      vh = max 6 (rows `div` 2)
      textRows = max 1 (vh - 2)
      vTop = rows - vh + 1
  return (rows, cols, vh, textRows, vTop)

readKey :: Handle -> IO Key
readKey h = do
  c <- hGetChar h
  case c of
    '\ESC' -> do
      pending <- hReady h
      if not pending
        then return KEsc
        else do
          c1 <- hGetChar h
          if c1 /= '['
            then return KEsc
            else do
              c2 <- hGetChar h
              case c2 of
                'A' -> return KUp
                'B' -> return KDown
                '5' -> swallowTilde >> return KPageUp
                '6' -> swallowTilde >> return KPageDown
                _   -> return KOther
    '\r'   -> return KEnter
    '\n'   -> return KEnter
    '\DEL' -> return KBackspace
    '\b'   -> return KBackspace
    _      -> return (KChar c)
  where
    swallowTilde = do
      pending <- hReady h
      when pending $ hGetChar h >> return ()

uiLoop :: Terminal -> FilePath -> Document -> IORef [PdfWarning] -> IO ()
uiLoop term path doc warningsRef = do
  linesRef <- newIORef ([] :: [Text])
  pagesLoadedRef <- newIORef (0 :: Int)
  totalPagesRef <- newIORef (0 :: Int)
  doneRef <- newIORef False
  scrollRef <- newIORef (initialScrollState (termTextRows term))
  redrawRef <- newIORef True
  modeRef <- newIORef ModeView
  searchRef <- newIORef (Nothing :: Maybe (String, Regex))
  matchRef <- newIORef (Nothing :: Maybe Int)
  msgRef <- newIORef (Nothing :: Maybe String)

  let producer = do
        ws <- pdfToTextStreamDoc doc $ \pg total bs -> do
          writeIORef totalPagesRef total
          modifyIORef' linesRef $ appendChunk (decodeLenient bs)
          writeIORef pagesLoadedRef pg
          ls <- readIORef linesRef
          modifyIORef' scrollRef (\st -> st {scrollTotalLines = length ls})
          writeIORef redrawRef True
        writeIORef warningsRef ws
        writeIORef doneRef True
        writeIORef redrawRef True

  let syncTotal = do
        ls <- readIORef linesRef
        modifyIORef' scrollRef (\st -> st {scrollTotalLines = length ls})
        return ls

      -- Jump forward (or backward) to the next line matching the active pattern.
      searchStep forward = do
        msearch <- readIORef searchRef
        case msearch of
          Nothing -> writeIORef msgRef (Just "No previous search")
          Just (_, re) -> do
            ls <- syncTotal
            st <- readIORef scrollRef
            cur <- readIORef matchRef
            let p = matchTest re . T.unpack
                top = scrollTopLine st
                found
                  | forward = searchForwardFrom p (maybe (top + 1) (+ 1) cur) ls
                  | otherwise = searchBackwardFrom p (maybe (top - 1) (subtract 1) cur) ls
            case found of
              Nothing -> writeIORef msgRef (Just "Pattern not found")
              Just i -> do
                writeIORef matchRef (Just i)
                modifyIORef' scrollRef (clampScrollTop i)

      startSearch q
        | null q = do
            msearch <- readIORef searchRef
            case msearch of
              Just _ -> searchStep True
              Nothing -> return ()
        | otherwise =
            case makeRegexM q :: Maybe Regex of
              Nothing -> writeIORef msgRef (Just "Invalid pattern")
              Just re -> do
                writeIORef searchRef (Just (q, re))
                writeIORef matchRef Nothing
                searchStep True

      handleViewKey key = case key of
        KChar 'q' -> return True
        KChar '\ETX' -> return True
        KChar '/' -> writeIORef modeRef (ModeInput "") >> return False
        KChar 'n' -> searchStep True >> return False
        KChar 'N' -> searchStep False >> return False
        _ -> do
          _ <- syncTotal
          modifyIORef' scrollRef (moveKey (termTextRows term) key)
          return False

      handleInputKey q key = do
        case key of
          KEsc -> writeIORef modeRef ModeView
          KEnter -> do
            writeIORef modeRef ModeView
            startSearch q
          KBackspace ->
            writeIORef modeRef $
              if null q then ModeView else ModeInput (init q)
          KChar c
            | c == '\ETX' -> writeIORef modeRef ModeView
            | isPrint c -> writeIORef modeRef (ModeInput (q ++ [c]))
          _ -> return ()
        return False

  let loop = do
        needRedraw <- readIORef redrawRef
        when needRedraw $ do
          writeIORef redrawRef False
          ls <- readIORef linesRef
          pg <- readIORef pagesLoadedRef
          total <- readIORef totalPagesRef
          done <- readIORef doneRef
          scroll <- readIORef scrollRef
          mode <- readIORef modeRef
          msg <- readIORef msgRef
          msearch <- readIORef searchRef
          drawFrame term path scroll ls pg total done mode msg (fmap snd msearch)
        ready <- hReady (termStdin term)
        if ready
          then do
            key <- readKey (termStdin term)
            writeIORef msgRef Nothing
            mode <- readIORef modeRef
            quit <- case mode of
              ModeView -> handleViewKey key
              ModeInput q -> handleInputKey q key
            writeIORef redrawRef True
            if quit then return () else loop
          else do
            threadDelay 50000
            loop

  tid <- forkIO producer
  loop `finally` killThread tid

moveKey :: Int -> Key -> ScrollState -> ScrollState
moveKey textRows key st = case key of
  KChar 'j' -> scrollBy 1 st
  KChar ' ' -> scrollBy 1 st
  KChar 'k' -> scrollBy (-1) st
  KDown     -> scrollBy 1 st
  KUp       -> scrollBy (-1) st
  KPageDown -> scrollBy textRows st
  KPageUp   -> scrollBy (-textRows) st
  KChar 'g' -> scrollToTop st
  KChar 'G' -> scrollToEnd st
  KChar 'd' -> scrollHalfPageDown st
  KChar 'u' -> scrollHalfPageUp st
  _         -> st

-- | Line buffer invariant: the last element is the still-open line
-- (empty when the previous chunk ended with a newline).
appendChunk :: Text -> [Text] -> [Text]
appendChunk chunk [] = T.split (== '\n') chunk
appendChunk chunk ls =
  case T.split (== '\n') chunk of
    (c : cs) -> init ls ++ (last ls <> c) : cs
    [] -> ls

decodeLenient :: BSL.ByteString -> Text
decodeLenient bs =
  TE.decodeUtf8With (\_ _ -> Just '\xfffd') (BSL.toStrict bs)

drawFrame ::
  Terminal -> FilePath -> ScrollState -> [Text] ->
  Int -> Int -> Bool -> UiMode -> Maybe String -> Maybe Regex -> IO ()
drawFrame term path scroll ls pg total done mode msg mre = do
  let (start, end) = visibleLineRange scroll
      visible = take (end - start) (drop start ls)
      status = case mode of
        ModeInput q -> "/" ++ q
        ModeView -> case msg of
          Just m -> " " ++ m
          Nothing ->
            " page "
              ++ show (if total > 0 then pg else 0)
              ++ "/"
              ++ show total
              ++ (if done then "" else "...")
              ++ " | line "
              ++ show (statusLineNumber scroll)
              ++ "/"
              ++ show (max (length ls) 1)
              ++ " | j/k \x2193/\x2191 d/u g/G /:search n/N q:quit"
  drawBarRow term (termViewportTop term) (" " ++ path)
  mapM_ (drawTextRow term mre) (zip [0 ..] visible)
  let filled = length visible
      statusRow = termViewportTop term + termViewportH term - 1
  mapM_ (\i -> drawTextRow term Nothing (i, "")) [filled .. termTextRows term - 1]
  drawBarRow term statusRow status
  -- During search input, park the visible cursor after the query so IME
  -- composition windows anchor to the right spot.
  case mode of
    ModeInput _ -> do
      let col = min (termCols term) (stringDisplayWidth status + 1)
      hPutStr (termStdout term) $
        "\ESC[" ++ show statusRow ++ ";" ++ show col ++ "H\ESC[?25h"
    ModeView ->
      hPutStr (termStdout term) "\ESC[?25l"
  hFlush (termStdout term)

drawTextRow :: Terminal -> Maybe Regex -> (Int, Text) -> IO ()
drawTextRow term mre (i, txt) = do
  let row = termViewportTop term + 1 + i
      clipped = clipToDisplayWidth (termCols term) (T.unpack txt)
      rendered = maybe clipped (`highlightMatches` clipped) mre
  hPutStr (termStdout term) $ "\ESC[" ++ show row ++ ";1H\ESC[K" ++ rendered

-- | Wrap every regex match on the (already clipped) line in reverse video.
highlightMatches :: Regex -> String -> String
highlightMatches re s = go 0 s spans
  where
    spans = [(off, len) | ma <- matchAll re s, let (off, len) = ma ! 0, len > 0]
    go _ rest [] = rest
    go pos rest ((off, len) : more) =
      let (before, rest') = splitAt (off - pos) rest
          (hit, rest'') = splitAt len rest'
       in before ++ "\ESC[7m" ++ hit ++ "\ESC[0m" ++ go (off + len) rest'' more

-- | Full-width reverse-video bar (used for the top border and the status line).
drawBarRow :: Terminal -> Int -> String -> IO ()
drawBarRow term row content = do
  let padded = padToDisplayWidth (termCols term) content
  hPutStr (termStdout term) $
    "\ESC[" ++ show row ++ ";1H\ESC[7m" ++ padded ++ "\ESC[0m"

clearViewport :: Terminal -> IO ()
clearViewport term =
  mapM_ clearRow [termViewportTop term .. termViewportTop term + termViewportH term - 1]
  where
    clearRow r = hPutStr (termStdout term) $ "\ESC[" ++ show r ++ ";1H\ESC[K"
