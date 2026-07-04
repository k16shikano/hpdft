{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Definition

import PDF.Object
import PDF.DocumentStructure
import PDF.PDFIO
import PDF.Outlines
import PDF.Encrypt (Security)
import PDF.Text (initstate, pdfToTextWithWarnings)
import PDF.Error (PdfError(..), PdfResult, PdfWarning(..), renderPdfWarning)

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, IOException, ioError, throwIO)

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Lazy as TL (unpack)
import Data.Text.Lazy.Encoding as TL
import Data.List (nub, find, intercalate)
import Data.Maybe (fromMaybe)

import Options.Applicative
import Data.Semigroup ((<>))

import Text.Regex.Base.RegexLike ( makeRegex )
import Text.Regex.TDFA.String    ( regexec )

import Options.Applicative (strOption)
import Control.Monad (when)
import PDF.Definition (Obj(PdfStream))

import qualified Paths_hpdft as Autogen (version)
import Data.Version (showVersion)

main :: IO ()
main = hpdft =<< execParser opts
  where
    opts = info (options <**> helper <**> (simpleVersioner versionInfo))
      (fullDesc
       <> header versionInfo)

versionInfo = "hpdft - a PDF to text converter, version " <> showVersion Autogen.version

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

data CmdOpt = CmdOpt {
  page :: Int,
  ref  :: Int,
  grep :: String,
  refs :: Bool,
  pdftitle :: Bool,
  pdfinfo :: Bool,
  pdfoutline :: Bool,
  trailer :: Bool,
  password :: String,
  file :: FilePath
  }

options :: Parser CmdOpt
options = CmdOpt
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

hpdft :: CmdOpt -> IO ()
hpdft cmd@CmdOpt{password=pw, file=fn, page=pg, ref=rf, grep=gr, refs=rs, pdftitle=tt, pdfinfo=ii, pdfoutline=oo, trailer=tr} =
  withFile fn $
  let mpw = Just pw
  in case () of
    _ | pg==0 && rf==0 && null gr && not rs && not tt && not ii && not oo && not tr -> pdfToText fn mpw
      | pg==0 && rf==0 && null gr && not rs && tt      -> showTitle fn mpw
      | pg==0 && rf==0 && null gr && not rs && ii      -> showInfo fn mpw
      | pg==0 && rf==0 && null gr && not rs && oo      -> showOutlines fn mpw
      | pg==0 && rf==0 && null gr && not rs && tr      -> showTrailer fn
      | pg==0 && rf==0 && null gr && rs                -> showRefs fn mpw
      | rf==0 && null gr && pg/=0                      -> showPage fn mpw pg
      | pg==0 && null gr && rf/=0                      -> showContent fn mpw rf
      | pg==0 && rf==0 && not (null gr)                -> grepPDF fn mpw gr
      | otherwise -> return ()

pdfToText :: FilePath -> Maybe String -> IO ()
pdfToText filename mpw = do
  (txt, ws) <- runOrDie (pdfToTextWithWarnings filename mpw)
  printWarnings ws
  BSL.putStrLn txt

data  PageTree = Nop | Page Int | Pages [PageTree]
                 deriving Show

showRefs :: FilePath -> Maybe String -> IO ()
showRefs filename mpw = do
  root <- runOrDie (getRootRef filename)
  (objs, _) <- runOrDie (getPDFObjFromFile filename mpw)
  print $ pageTreeToList $ pageorder root objs

refByPage :: FilePath -> Maybe String -> IO [Int]
refByPage filename mpw = do
  root <- runOrDie (getRootRef filename)
  (objs, _) <- runOrDie (getPDFObjFromFile filename mpw)
  return $ pageTreeToList $ pageorder root objs

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

showPage :: FilePath -> Maybe String -> Int -> IO ()
showPage filename mpw page = do
  (objs, sec) <- runOrDie (getPDFObjFromFile filename mpw)
  root <- runOrDie (getRootRef filename)
  let pagetree = pageTreeToList $ pageorder root objs
  case length pagetree >= page of
    True -> contentByRefObjs sec objs $ pagetree !! (page - 1)
    False -> putStrLn $ "hpdft: No Page "++(show page)

contentByRefObjs :: Maybe Security -> PDFObjIndex -> Int -> IO ()
contentByRefObjs sec objs ref = do
  obj <- runOrDie (getObjectByRef ref objs)
  let (txt, ws) = contentInObject sec obj objs
  printWarnings ws
  BSL.putStrLn txt
  where contentInObject sec' obj' objs' =
          case findDictOfType "/Page" obj' of
            Just dict -> pageStream dict sec' objs'
            Nothing -> ("", [])

pageStream :: Dict -> Maybe Security -> PDFObjIndex -> (BSL.ByteString, [PdfWarning])
pageStream dict sec objs =
  case contentsStream dict initstate sec objs of
    Right (s, ws) -> (s, ws)
    Left _ -> ("", [])

showContent :: FilePath -> Maybe String -> Int -> IO ()
showContent filename mpw ref = do
  (objs, sec) <- runOrDie (getPDFObjFromFile filename mpw)
  obj <- runOrDie (getObjectByRef ref objs)
  let d = fromMaybe [] $ findDict obj
  if hasStream obj
    then
      if hasSubtype d
        then printStreamWithDict sec ref d obj
        else do
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

    hasSubtype d = case find isSubtype d of
                       Just _ -> True
                       Nothing -> False
    isSubtype (PdfName "/Subtype", _) = True
    isSubtype x = False

    printStreamWithDict :: Maybe Security -> Int -> Dict -> [Obj] -> IO ()
    printStreamWithDict sec' ref' d obj = do
      putStrLn $ ppObj (PdfDict d)
      strm <- runOrDie (getStream sec' ref' True obj)
      BSL.putStrLn strm

showTitle :: FilePath -> Maybe String -> IO ()
showTitle filename mpw = do
  d <- runOrDie (getInfo filename mpw)
  let title = 
        case findObjFromDict d "/Title" of
          Just (PdfText s) -> s
          Just x -> ppObj x
          Nothing -> "No title anyway"
  putStrLn title

showInfo :: FilePath -> Maybe String -> IO ()
showInfo filename mpw = do
  d <- runOrDie (getInfo filename mpw)
  putStrLn $ ppObj (PdfDict d)

showOutlines :: FilePath -> Maybe String -> IO ()
showOutlines filename mpw = do
  d <- runOrDie (getOutlines filename mpw)
  putStrLn $ show d

showTrailer :: FilePath -> IO ()
showTrailer filename = do
  d <- runOrDie (getTrailer filename)
  putStrLn $ ppDictEntries d

grepPDF :: FilePath -> Maybe String -> String -> IO ()
grepPDF filename mpw re = do
  root <- runOrDie (getRootRef filename)
  (objs, sec) <- runOrDie (getPDFObjFromFile filename mpw)
  mapM_
    (\(ref, pagenm) -> grepByPage pagenm re (contentInObjs sec objs ref))
    $ zip (pageTreeToList $ pageorder root objs) [1..]
  
  where
    contentInObjs sec' objs' ref =
      case findObjsByRef ref objs' of
        Just obj -> case findDictOfType "/Page" obj of
                      Just dict -> pageStream dict sec' objs'
                      Nothing -> ("", [])
        Nothing -> ("", [])

    grepByPage :: Int -> String -> (BSL.ByteString, [PdfWarning]) -> IO ()
    grepByPage pagenm re (txt, _) = do
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
