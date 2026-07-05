{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Definition

import PDF.Object
import PDF.Document (Document(..), openDocument, docRootRef, docInfoDict)
import PDF.DocumentStructure
import PDF.PDFIO
import PDF.Outlines
import PDF.Encrypt (Security)
import PDF.Text (pdfToTextWithWarnings, pdfToTextGeomBSWith, pdfToTextTaggedBSWith, pageTextGeomWith)
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions)
import PDF.Diff (TextChange(..), compareDocuments)
import PDF.Image (extractPageImagesToDir)
import PDF.Error (PdfError(..), PdfResult, PdfWarning(..), renderPdfWarning)

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, IOException, ioError)
import Control.Monad (when)

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Lazy as TL (unpack)
import Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T
import Data.List (find, intercalate)

import Options.Applicative
import Data.Semigroup ((<>))

import Text.Regex.Base.RegexLike ( makeRegex )
import Text.Regex.TDFA.String    ( regexec )

import PDF.Definition (Obj(PdfStream))

import qualified Paths_hpdft as Autogen (version)
import Data.Version (showVersion)

deprecationMsg :: String
deprecationMsg =
  "hpdft: flat flags are deprecated; use 'hpdft extract ...'"

subcommandNames :: [String]
subcommandNames =
  [ "extract", "diff", "info", "title", "toc", "trailer", "object", "refs", "grep" ]

main :: IO ()
main = do
  args <- getArgs
  let useSubcommands = not (null args) && head args `elem` subcommandNames
      isMeta = any (`elem` args) ["--help", "-h", "--version", "-V"]
      parserExtras = helper <**> simpleVersioner versionInfo
  cmd <- if useSubcommands
         then execParser (info (commandParser <**> parserExtras) (fullDesc <> header versionInfo))
         else if isMeta
              then execParser (info (legacyCmd <**> parserExtras) (fullDesc <> header versionInfo))
              else do
                hPutStrLn stderr deprecationMsg
                execParser (info (legacyCmd <**> parserExtras) (fullDesc <> header versionInfo))
  runCmd cmd

versionInfo :: String
versionInfo = "hpdft - a PDF to text converter, version " <> showVersion Autogen.version

-- | Top-level command dispatch.
data Cmd
  = CmdExtract ExtractSub
  | CmdDiff DiffOpt
  | CmdInfo FilePath (Maybe String)
  | CmdTitle FilePath (Maybe String)
  | CmdToc FilePath (Maybe String)
  | CmdTrailer FilePath
  | CmdObject Int FilePath (Maybe String)
  | CmdRefs FilePath (Maybe String)
  | CmdGrep String FilePath (Maybe String)

data ExtractSub
  = SubExtractText ExtractOpt
  | SubExtractImages ImagesOpt

data ExtractOpt = ExtractOpt
  { eoPage      :: Int
  , eoGeom      :: Bool
  , eoTagged    :: Bool
  , eoLegacy    :: Bool
  , eoFootnotes :: Bool
  , eoRuby      :: Bool
  , eoPassword  :: String
  , eoFile      :: FilePath
  }

data ImagesOpt = ImagesOpt
  { ioPage     :: Int
  , ioOut      :: FilePath
  , ioPassword :: String
  , ioFile     :: FilePath
  }

data DiffOpt = DiffOpt
  { doGeom      :: Bool
  , doLegacy    :: Bool
  , doRuby      :: Bool
  , doJson      :: Bool
  , doPassword  :: String
  , doFileA     :: FilePath
  , doFileB     :: FilePath
  }

commandParser :: Parser Cmd
commandParser = subparser
  (  command "extract" (info extractCommand (progDesc "Extract text from PDF"))
  <> command "diff" (info diffCommand (progDesc "Compare two PDFs (paragraph-level diff)"))
  <> command "info" (info infoCommand (progDesc "Show PDF metadata"))
  <> command "title" (info titleCommand (progDesc "Show document title"))
  <> command "toc" (info tocCommand (progDesc "Show table of contents"))
  <> command "trailer" (info trailerCommand (progDesc "Show PDF trailer"))
  <> command "object" (info objectCommand (progDesc "Show object by reference"))
  <> command "refs" (info refsCommand (progDesc "Show page object references"))
  <> command "grep" (info grepCommand (progDesc "Search text in PDF"))
  )

extractCommand :: Parser Cmd
extractCommand = CmdExtract <$> extractSub

extractSub :: Parser ExtractSub
extractSub =
  subparser
    (  command "text" (info (SubExtractText <$> extractOpts) (progDesc "Extract text (explicit)"))
    <> command "images" (info (SubExtractImages <$> imagesOpts) (progDesc "Extract image XObjects from a page"))
    )
  <|> SubExtractText <$> extractOpts

imagesOpts :: Parser ImagesOpt
imagesOpts = ImagesOpt
  <$> option auto
      ( long "page"
        <> short 'p'
        <> metavar "PAGE"
        <> help "Page number (1-based, required)" )
  <*> strOption
      ( long "output"
        <> short 'o'
        <> value "."
        <> metavar "DIR"
        <> help "Output directory (default: current directory)" )
  <*> strOption
      ( long "password"
        <> short 'P'
        <> value ""
        <> metavar "PASSWORD"
        <> help "Password for encrypted PDF" )
  <*> strArgument
      ( help "input pdf file"
        <> metavar "FILE"
        <> action "file" )
extractOpts :: Parser ExtractOpt
extractOpts = ExtractOpt
  <$> option auto
      ( long "page"
        <> short 'p'
        <> value 0
        <> metavar "PAGE"
        <> help "Page number (1-based; 0 = all pages)" )
  <*> switch
      ( long "geom"
        <> help "Extract text using geometry-based layout" )
  <*> switch
      ( long "tagged"
        <> help "Extract text using tagged PDF structure" )
  <*> switch
      ( long "legacy"
        <> help "Extract text using the pre-0.3 stream-order extractor" )
  <*> switch
      ( long "footnotes"
        <> help "Inline footnote bodies at their anchors as <footnote> tags (geometry pipeline)" )
  <*> switch
      ( long "ruby"
        <> help "Embed ruby in Aozora bunko notation (geometry/tagged pipeline)" )
  <*> strOption
      ( long "password"
        <> short 'P'
        <> value ""
        <> metavar "PASSWORD"
        <> help "Password for encrypted PDF" )
  <*> strArgument
      ( help "input pdf file"
        <> metavar "FILE"
        <> action "file" )

diffCommand :: Parser Cmd
diffCommand = CmdDiff <$> diffOpts

diffOpts :: Parser DiffOpt
diffOpts = DiffOpt
  <$> switch
      ( long "geom"
        <> help "Use geometry-based layout (default for diff)" )
  <*> switch
      ( long "legacy"
        <> help "Ignored for diff (geometry pipeline always used)" )
  <*> switch
      ( long "ruby"
        <> help "Embed ruby in Aozora bunko notation during layout" )
  <*> switch
      ( long "json"
        <> help "Emit JSON instead of human-readable diff" )
  <*> strOption
      ( long "password"
        <> short 'P'
        <> value ""
        <> metavar "PASSWORD"
        <> help "Password for encrypted PDF (applied to both files)" )
  <*> strArgument
      ( help "first PDF file"
        <> metavar "FILE_A"
        <> action "file" )
  <*> strArgument
      ( help "second PDF file"
        <> metavar "FILE_B"
        <> action "file" )

passwordOpt :: Parser String
passwordOpt = strOption
  ( long "password"
    <> short 'P'
    <> value ""
    <> metavar "PASSWORD"
    <> help "Password for encrypted PDF" )

fileArg :: Parser FilePath
fileArg = strArgument
  ( help "input pdf file"
    <> metavar "FILE"
    <> action "file" )

maybePassword :: String -> Maybe String
maybePassword pw = if null pw then Nothing else Just pw

infoCommand :: Parser Cmd
infoCommand = CmdInfo <$> fileArg <*> (maybePassword <$> passwordOpt)

titleCommand :: Parser Cmd
titleCommand = CmdTitle <$> fileArg <*> (maybePassword <$> passwordOpt)

tocCommand :: Parser Cmd
tocCommand = CmdToc <$> fileArg <*> (maybePassword <$> passwordOpt)

trailerCommand :: Parser Cmd
trailerCommand = CmdTrailer <$> fileArg

objectCommand :: Parser Cmd
objectCommand = CmdObject
  <$> option auto
      ( long "ref"
        <> short 'r'
        <> metavar "REF"
        <> help "Object reference number" )
  <*> fileArg
  <*> (maybePassword <$> passwordOpt)

refsCommand :: Parser Cmd
refsCommand = CmdRefs <$> fileArg <*> (maybePassword <$> passwordOpt)

grepCommand :: Parser Cmd
grepCommand = CmdGrep
  <$> strOption
      ( long "grep"
        <> short 'g'
        <> metavar "REGEXP"
        <> help "Regular expression to search for" )
  <*> fileArg
  <*> (maybePassword <$> passwordOpt)

-- Legacy flat-flag parser (deprecated).
legacyCmd :: Parser Cmd
legacyCmd = legacyToCmd <$> legacyOptions

legacyOptions :: Parser LegacyOpt
legacyOptions = LegacyOpt
  <$> option auto
      ( long "page"
        <> short 'p'
        <> value 0
        <> metavar "PAGE"
        <> help "Page number (nomble)" )
  <*> option auto
      ( long "ref"
        <> short 'r'
        <> value 0
        <> metavar "REF"
        <> help "Object reference" )
  <*> strOption
      ( long "grep"
        <> short 'g'
        <> value ""
        <> metavar "RegExp"
        <> help "grep PDF" )
  <*> switch
      ( long "refs"
        <> short 'R'
        <> help "Show object references in page order" )
  <*> switch
      ( long "geom"
        <> help "Extract text using geometry-based layout" )
  <*> switch
      ( long "tagged"
        <> help "Extract text using tagged PDF structure" )
  <*> switch
      ( long "legacy"
        <> help "Extract text using the pre-0.3 stream-order extractor" )
  <*> switch
      ( long "footnotes"
        <> help "Inline footnote bodies at their anchors as <footnote> tags (geometry pipeline)" )
  <*> switch
      ( long "ruby"
        <> help "Embed ruby in Aozora bunko notation (geometry/tagged pipeline)" )
  <*> switch
      ( long "title"
        <> short 'T'
        <> help "Show title (from metadata)" )
  <*> switch
      ( long "info"
        <> short 'I'
        <> help "Show PDF metainfo" )
  <*> switch
      ( long "toc"
        <> short 'O'
        <> help "Show table of contents (from metadata) " )
  <*> switch
      ( long "trailer"
        <> help "Show the trailer of PDF" )
  <*> strOption
      ( long "password"
        <> short 'P'
        <> value ""
        <> metavar "PASSWORD"
        <> help "Password for encrypted PDF" )
  <*> strArgument
      ( help "input pdf file"
        <> metavar "FILE"
        <> action "file" )

data LegacyOpt = LegacyOpt
  { loPage      :: Int
  , loRef       :: Int
  , loGrep      :: String
  , loRefs      :: Bool
  , loGeom      :: Bool
  , loTagged    :: Bool
  , loLegacy    :: Bool
  , loFootnotes :: Bool
  , loRuby      :: Bool
  , loTitle     :: Bool
  , loInfo      :: Bool
  , loToc       :: Bool
  , loTrailer   :: Bool
  , loPassword  :: String
  , loFile      :: FilePath
  }

legacyToCmd :: LegacyOpt -> Cmd
legacyToCmd LegacyOpt{loPage=pg, loRef=rf, loGrep=gr, loRefs=rs, loGeom=gm,
                      loTagged=tg, loLegacy=lg, loFootnotes=fnn, loRuby=rb,
                      loTitle=tt, loInfo=ii, loToc=oo, loTrailer=tr,
                      loPassword=pw, loFile=fn} =
  let mpw = maybePassword pw
      noMode = not gm && not tg && not lg
      extract = SubExtractText (ExtractOpt pg gm tg lg fnn rb pw fn)
  in case () of
    _ | pg==0 && rf==0 && null gr && not rs && lg && not gm && not tg && not tt && not ii && not oo && not tr ->
        CmdExtract extract
      | pg==0 && rf==0 && null gr && not rs && gm && not tg && not lg && not tt && not ii && not oo && not tr ->
        CmdExtract extract
      | pg==0 && rf==0 && null gr && not rs && (tg || noMode) && not gm && not lg && not tt && not ii && not oo && not tr ->
        CmdExtract extract
      | pg==0 && rf==0 && null gr && not rs && noMode && tt ->
        CmdTitle fn mpw
      | pg==0 && rf==0 && null gr && not rs && noMode && ii ->
        CmdInfo fn mpw
      | pg==0 && rf==0 && null gr && not rs && noMode && oo ->
        CmdToc fn mpw
      | pg==0 && rf==0 && null gr && not rs && noMode && tr ->
        CmdTrailer fn
      | pg==0 && rf==0 && null gr && rs && noMode ->
        CmdRefs fn mpw
      | rf==0 && null gr && pg/=0 ->
        CmdExtract extract
      | pg==0 && null gr && rf/=0 ->
        CmdObject rf fn mpw
      | pg==0 && rf==0 && not (null gr) ->
        CmdGrep gr fn mpw
      | otherwise ->
        CmdExtract extract

runCmd :: Cmd -> IO ()
runCmd cmd = case cmd of
  CmdExtract sub -> runExtractSub sub
  CmdDiff opt -> runDiff opt
  CmdInfo fn mpw -> withFile fn $ showInfo fn mpw
  CmdTitle fn mpw -> withFile fn $ showTitle fn mpw
  CmdToc fn mpw -> withFile fn $ showOutlines fn mpw
  CmdTrailer fn -> withFile fn $ showTrailer fn
  CmdObject rf fn mpw -> withFile fn $ showContent fn mpw rf
  CmdRefs fn mpw -> withFile fn $ showRefs fn mpw
  CmdGrep re fn mpw -> withFile fn $ grepPDF defaultLayoutOptions fn mpw re

runExtractSub :: ExtractSub -> IO ()
runExtractSub sub = case sub of
  SubExtractText opt -> runExtractText opt
  SubExtractImages opt -> runExtractImages opt

runExtractText :: ExtractOpt -> IO ()
runExtractText ExtractOpt{eoPage=pg, eoGeom=gm, eoTagged=tg, eoLegacy=lg,
                      eoFootnotes=fnn, eoRuby=rb, eoPassword=pw, eoFile=fn} =
  withFile fn $
  let mpw = maybePassword pw
      lopts = defaultLayoutOptions {optFootnotes = fnn, optRuby = rb}
      noMode = not gm && not tg && not lg
  in if pg /= 0
     then showPage lopts fn mpw pg
     else case () of
       _ | lg && not gm && not tg -> pdfToText fn mpw
         | gm && not tg && not lg -> pdfToTextGeom lopts fn mpw
         | tg || noMode           -> pdfToTextTagged lopts fn mpw
         | otherwise              -> pdfToTextTagged lopts fn mpw

runExtractImages :: ImagesOpt -> IO ()
runExtractImages ImagesOpt{ioPage=pg, ioOut=out, ioPassword=pw, ioFile=fn} =
  withFile fn $
  if pg < 1
    then do
      hPutStrLn stderr "hpdft: extract images requires -p PAGE (1-based)"
      exitWith (ExitFailure 1)
    else do
      doc <- runOrDie (openDocument fn (maybePassword pw))
      paths <- runOrDie (extractPageImagesToDir doc pg out)
      mapM_ putStrLn paths

runDiff :: DiffOpt -> IO ()
runDiff DiffOpt{doRuby=rb, doJson=json, doPassword=pw, doFileA=fa, doFileB=fb} =
  withFile fa $
  withFile fb $
  let mpw = maybePassword pw
      lopts = defaultLayoutOptions {optRuby = rb}
  in do
    docA <- runOrDie (openDocument fa mpw)
    docB <- runOrDie (openDocument fb mpw)
    changes <- runOrDie (return (compareDocuments lopts docA docB))
    if json
      then putStrLn (renderDiffJson changes)
      else mapM_ putStrLn (renderDiffHuman changes)

renderDiffHuman :: [TextChange] -> [String]
renderDiffHuman = map renderOne
  where
    renderOne (PageCountMismatch pa pb) =
      "page count mismatch: " ++ show pa ++ " vs " ++ show pb
    renderOne TextChange{changePageA = pa, changePageB = pb,
                         changeParaA = pxa, changeParaB = pxb,
                         changeOld = old, changeNew = new} =
      unlines
        ( pageLine
        : paraLine
        : ("- old: " ++ T.unpack old) : ("+ new: " ++ T.unpack new) : []
        )
      where
        pageLine =
          case (pa, pb) of
            (Just a, Just b) | a == b -> "page " ++ show a ++ ":"
            (Just a, Just b) -> "page " ++ show a ++ " vs " ++ show b ++ ":"
            (Just a, Nothing) -> "page " ++ show a ++ " (only in first file):"
            (Nothing, Just b) -> "page " ++ show b ++ " (only in second file):"
            _ -> "page ?:"
        paraLine =
          case (pxa, pxb) of
            (Just a, Just b) | a == b -> "para " ++ show (a + 1) ++ ":"
            (Just a, Just b) -> "para " ++ show (a + 1) ++ " vs " ++ show (b + 1) ++ ":"
            (Just a, Nothing) -> "para " ++ show (a + 1) ++ ":"
            (Nothing, Just b) -> "para " ++ show (b + 1) ++ ":"
            _ -> "para ?:"

renderDiffJson :: [TextChange] -> String
renderDiffJson changes = "[" ++ intercalate "," (map encodeChange changes) ++ "]"
  where
    encodeChange (PageCountMismatch pa pb) =
      "{\"type\":\"pageCountMismatch\",\"pagesA\":" ++ show pa
        ++ ",\"pagesB\":" ++ show pb ++ "}"
    encodeChange TextChange{changePageA = pa, changePageB = pb,
                            changeParaA = pxa, changeParaB = pxb,
                            changeOld = old, changeNew = new} =
      "{\"type\":\"textChange\""
        ++ maybeField "pageA" pa
        ++ maybeField "pageB" pb
        ++ maybeField "paraA" pxa
        ++ maybeField "paraB" pxb
        ++ ",\"old\":" ++ jsonString old
        ++ ",\"new\":" ++ jsonString new
        ++ "}"
    maybeField _ Nothing = ""
    maybeField k (Just v) = ",\"" ++ k ++ "\":" ++ show v
    jsonString t =
      "\"" ++ concatMap esc (T.unpack t) ++ "\""
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c = [c]

describeError :: PdfError -> String
describeError (ParseError msg _) = "parse error: " ++ msg
describeError (BrokenXref msg) = "broken cross-reference: " ++ msg
describeError (MissingObject n) = "missing object: " ++ show n ++ " 0 R"
describeError (MissingKey key ctx) = "missing key " ++ key ++ " in " ++ ctx
describeError (UnsupportedFeature msg) = "unsupported feature: " ++ msg
describeError (DecryptionError msg) =
  "cannot decrypt: " ++ msg ++ ". Use -P to supply a password."
describeError (FontError n msg) = "font error in object " ++ show n ++ ": " ++ msg

printWarnings :: [PdfWarning] -> IO ()
printWarnings = mapM_ (hPutStrLn stderr . ("hpdft: warning: " ++) . renderPdfWarning)

runOrDie :: IO (PdfResult a) -> IO a
runOrDie action = do
  result <- action
  case result of
    Right a -> return a
    Left err -> do
      hPutStrLn stderr ("hpdft: " ++ describeError err)
      exitWith (ExitFailure 1)

withFile :: FilePath -> IO () -> IO ()
withFile fp action =
  action `catch` \e -> do
    if isDoesNotExistError (e :: IOException)
      then do
        hPutStrLn stderr ("hpdft: " ++ fp ++ ": does not exist")
        exitWith (ExitFailure 1)
      else ioError e

pdfToText :: FilePath -> Maybe String -> IO ()
pdfToText filename mpw = do
  (txt, ws) <- runOrDie (pdfToTextWithWarnings filename mpw)
  printWarnings ws
  BSL.putStrLn txt

pdfToTextGeom :: LayoutOptions -> FilePath -> Maybe String -> IO ()
pdfToTextGeom lopts filename mpw = do
  txt <- runOrDie (pdfToTextGeomBSWith lopts filename mpw)
  BSL.putStrLn txt

pdfToTextTagged :: LayoutOptions -> FilePath -> Maybe String -> IO ()
pdfToTextTagged lopts filename mpw = do
  txt <- runOrDie (pdfToTextTaggedBSWith lopts filename mpw)
  BSL.putStrLn txt

data PageTree = Nop | Page Int | Pages [PageTree]
  deriving Show

showRefs :: FilePath -> Maybe String -> IO ()
showRefs filename mpw = do
  doc <- runOrDie (openDocument filename mpw)
  root <- runOrDie (return (docRootRef doc))
  print $ pageTreeToList $ pageorder root (docObjs doc)

refByPage :: FilePath -> Maybe String -> IO [Int]
refByPage filename mpw = do
  doc <- runOrDie (openDocument filename mpw)
  root <- runOrDie (return (docRootRef doc))
  return $ pageTreeToList $ pageorder root (docObjs doc)

pageorder :: Int -> PDFObjIndex -> PageTree
pageorder parent objs =
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of
        Just pr -> pageorder pr objs
        Nothing -> Nop
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just kidsrefs -> Pages $ map (\f -> f objs) (map pageorder kidsrefs)
          Nothing -> Nop
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> Page parent
          Nothing -> Nop
    Nothing -> Nop

pageTreeToList :: PageTree -> [Int]
pageTreeToList (Pages ps) = concatMap pageTreeToList ps
pageTreeToList (Page n) = [n]
pageTreeToList Nop = []

showPage :: LayoutOptions -> FilePath -> Maybe String -> Int -> IO ()
showPage lopts filename mpw page = do
  doc <- runOrDie (openDocument filename mpw)
  root <- runOrDie (return (docRootRef doc))
  let pagetree = pageTreeToList $ pageorder root (docObjs doc)
  if page >= 1 && length pagetree >= page
    then do
      txt <- runOrDie (return (pageTextGeomWith lopts doc (pagetree !! (page - 1))))
      BSL.putStr txt
    else putStrLn $ "hpdft: No Page "++(show page)

showContent :: FilePath -> Maybe String -> Int -> IO ()
showContent filename mpw ref = do
  doc <- runOrDie (openDocument filename mpw)
  let objs = docObjs doc
      sec = docSecurity doc
  obj <- runOrDie (getObjectByRef ref objs)
  if hasStream obj
    then case findDict obj of
      Just d | hasSubtype d -> printStreamWithDict sec ref d obj
      _ -> do
        strm <- runOrDie (getStream sec ref False obj)
        BSL.putStrLn strm
    else do
      objs' <- runOrDie (getObjectByRef ref objs)
      putStrLn $ "[" ++ intercalate ", " (map ppObj objs') ++ "]"
  where

    hasStream obj = case find isStream obj of
      Just _ -> True
      Nothing -> False
    isStream (PdfStream _) = True
    isStream _             = False

    hasSubtype d = case findObjFromDict d "/Subtype" of
      Just _ -> True
      Nothing -> False

    printStreamWithDict :: Maybe Security -> Int -> Dict -> [Obj] -> IO ()
    printStreamWithDict sec' ref' d obj = do
      putStrLn $ ppObj (PdfDict d)
      strm <- runOrDie (getStream sec' ref' True obj)
      BSL.putStrLn strm

showTitle :: FilePath -> Maybe String -> IO ()
showTitle filename mpw = do
  doc <- runOrDie (openDocument filename mpw)
  d <- runOrDie (return (docInfoDict doc))
  let title =
        case findObjFromDict d "/Title" of
          Just (PdfText s) -> T.unpack s
          Just x -> ppObj x
          Nothing -> "No title anyway"
  putStrLn title

showInfo :: FilePath -> Maybe String -> IO ()
showInfo filename mpw = do
  doc <- runOrDie (openDocument filename mpw)
  d <- runOrDie (return (docInfoDict doc))
  putStrLn $ ppObj (PdfDict d)

showOutlines :: FilePath -> Maybe String -> IO ()
showOutlines filename mpw = do
  d <- runOrDie (getOutlines filename mpw)
  putStrLn $ show d

showTrailer :: FilePath -> IO ()
showTrailer filename = do
  doc <- runOrDie (openDocument filename Nothing)
  putStrLn $ ppDictEntries (docTrailer doc)

grepPDF :: LayoutOptions -> FilePath -> Maybe String -> String -> IO ()
grepPDF lopts filename mpw re = do
  doc <- runOrDie (openDocument filename mpw)
  root <- runOrDie (return (docRootRef doc))
  let objs = docObjs doc
  mapM_
    (\(ref, pagenm) -> grepByPage pagenm re (pageText doc ref))
    $ zip (pageTreeToList $ pageorder root objs) [1..]

  where
    pageText doc ref =
      case pageTextGeomWith lopts doc ref of
        Right txt -> txt
        Left _ -> ""

    grepByPage :: Int -> String -> BSL.ByteString -> IO ()
    grepByPage pagenm re txt = do
      let matched = filter (not . null) $ map (grepByLine re) $ BSL.lines txt
      when (not $ null matched) (showResult pagenm matched)
      return ()
      where
        showResult p m = do
          putStrLn $ "At page " <> show p <> "..."
          mapM (putStrLn . (" | " <>)) m
          return ()

    grepByLine :: String -> PDFStream -> String
    grepByLine re txt =
      case regexec (makeRegex re) $ TL.unpack $ TL.decodeUtf8 txt of
        Left _  -> ""
        Right m -> case m of
         Just (b, m, a, _) -> (b <> (highlight m) <> a)
         Nothing           -> ""

    highlight m = "\ESC[31m" <> m <> "\ESC[0m"
