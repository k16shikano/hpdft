{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli.Common (maybePassword)
import Cli.Grep (grepPDF)
import Cli.Misc
  ( runDiff
  , runExtractForm
  , runExtractImages
  , showContent
  , showInfo
  , showOutlines
  , showRefs
  , showTitle
  , showTrailer
  )
import Cli.Parser
  ( Cmd(..)
  , DiffOpt(..)
  , ExtractOpt(..)
  , FormOpt(..)
  , ImagesOpt(..)
  , LegacyOpt(..)
  )
import Cli.Text (runExtractText)
import Cli.View (runViewer)

import PDF.Layout (defaultLayoutOptions)

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Data.Maybe (listToMaybe)

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Paths_hpdft as Autogen (version)
import Data.Version (showVersion)

import Control.Monad (when)

deprecationMsg :: String
deprecationMsg =
  "hpdft: flat flags are deprecated; use subcommands (e.g. hpdft text, hpdft info)"

subcommandNames :: [String]
subcommandNames =
  [ "text", "image", "form"
  , "diff", "info", "title", "toc", "trailer", "object", "refs", "grep"
  ]

main :: IO ()
main = do
  args <- getArgs
  when (listToMaybe args == Just "extract") $ do
    hPutStrLn stderr "hpdft: 'extract' subcommand removed; use text, image, or form"
    exitWith (ExitFailure 1)
  let useSubcommands = case listToMaybe args of
        Just a | a `elem` subcommandNames -> True
        _ -> False
      isMeta = any (`elem` args) ["--help", "-h", "--version", "-V"]
      parserExtras = helper <**> simpleVersioner versionInfo
  cmd <- if useSubcommands
         then execParser (info (commandParser <**> parserExtras) (fullDesc <> header versionInfo))
         else if isMeta
              then execParser (info (legacyCmd <**> parserExtras) (fullDesc <> header versionInfo))
              else do
                when (legacyNeedsDeprecation args) $
                  hPutStrLn stderr deprecationMsg
                execParser (info (legacyCmd <**> parserExtras) (fullDesc <> header versionInfo))
  runCmd cmd

versionInfo :: String
versionInfo = "hpdft - a PDF to text converter, version " <> showVersion Autogen.version

runCmd :: Cmd -> IO ()
runCmd cmd = case cmd of
  CmdText opt -> runExtractText opt
  CmdView fn mpw h -> runViewer fn mpw h
  CmdImage opt -> runExtractImages opt
  CmdForm opt -> runExtractForm opt
  CmdDiff opt -> runDiff opt
  CmdInfo fn mpw -> showInfo fn mpw
  CmdTitle fn mpw -> showTitle fn mpw
  CmdToc fn mpw -> showOutlines fn mpw
  CmdTrailer fn -> showTrailer fn
  CmdObject rf fn mpw -> showContent fn mpw rf
  CmdRefs fn mpw -> showRefs fn mpw
  CmdGrep re fn mpw -> grepPDF defaultLayoutOptions fn mpw re

passwordOpt :: Parser String
passwordOpt = strOption
  ( long "password"
    <> short 'P'
    <> value ""
    <> metavar "PASSWORD"
    <> help "Password for encrypted PDF" )

heightOpt :: Parser (Maybe String)
heightOpt = optional $
  strOption
    ( long "height"
      <> metavar "HEIGHT"
      <> help "TUI viewport height in terminal rows, or N% (e.g. 50%%, 100%%)" )

fileArg :: Parser FilePath
fileArg = strArgument
  ( help "input pdf file"
    <> metavar "FILE"
    <> action "file" )

commandParser :: Parser Cmd
commandParser = subparser
  (  command "text" (info (CmdText <$> extractOpts) (progDesc "Extract text to stdout (tagged -> geometry)"))
  <> command "image" (info (CmdImage <$> imagesOpts) (progDesc "Extract image XObjects from a page"))
  <> command "form" (info (CmdForm <$> formOpts) (progDesc "List or extract top-level Form XObjects on a page"))
  <> command "diff" (info diffCommand (progDesc "Compare two PDFs (paragraph-level diff)"))
  <> command "info" (info infoCommand (progDesc "Show PDF metadata"))
  <> command "title" (info titleCommand (progDesc "Show document title"))
  <> command "toc" (info tocCommand (progDesc "Show table of contents"))
  <> command "trailer" (info trailerCommand (progDesc "Show PDF trailer"))
  <> command "object" (info objectCommand (progDesc "Show object by reference"))
  <> command "refs" (info refsCommand (progDesc "Show page object references"))
  <> command "grep" (info grepCommand (progDesc "Search text in PDF"))
  )

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
  <*> passwordOpt
  <*> fileArg

formOpts :: Parser FormOpt
formOpts = FormOpt
  <$> option auto
      ( long "page"
        <> short 'p'
        <> metavar "PAGE"
        <> help "Page number (1-based, required)" )
  <*> optional
      ( strOption
          ( long "name"
            <> short 'n'
            <> metavar "NAME"
            <> help "Top-level Form XObject name (e.g. Fm42); omit to list extractable names on stdout" ) )
  <*> strOption
      ( long "output"
        <> short 'o'
        <> value "."
        <> metavar "DIR"
        <> help "Output directory (default: current directory)" )
  <*> passwordOpt
  <*> fileArg

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
      ( long "quiet"
        <> help "Suppress page progress on stderr during --legacy streaming" )
  <*> switch
      ( long "footnotes"
        <> help "Inline footnote bodies at their anchors as <footnote> tags (geometry pipeline)" )
  <*> switch
      ( long "ruby"
        <> help "Embed ruby in Aozora bunko notation (geometry/tagged pipeline)" )
  <*> passwordOpt
  <*> fileArg

diffCommand :: Parser Cmd
diffCommand = CmdDiff <$> diffOpts

diffOpts :: Parser DiffOpt
diffOpts = DiffOpt
  <$> switch
      ( long "geom"
        <> help "Use geometry-based layout (default for diff)" )
  <*> switch
      ( long "ruby"
        <> help "Embed ruby in Aozora bunko notation during layout" )
  <*> switch
      ( long "json"
        <> help "Emit JSON instead of human-readable diff" )
  <*> passwordOpt
  <*> strArgument
      ( help "first PDF file"
        <> metavar "FILE_A"
        <> action "file" )
  <*> strArgument
      ( help "second PDF file"
        <> metavar "FILE_B"
        <> action "file" )

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

legacyCmd :: Parser Cmd
legacyCmd = legacyToCmd <$> legacyOptions

legacyOptions :: Parser LegacyOpt
legacyOptions = LegacyOpt
  <$> option auto
      ( long "page"
        <> short 'p'
        <> value 0
        <> metavar "PAGE"
        <> help "Page number (number)" )
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
      ( long "height"
        <> value ""
        <> metavar "HEIGHT"
        <> help "TUI viewport height in terminal rows, or N%% (default: half screen)" )
  <*> passwordOpt
  <*> strArgument
      ( help "input pdf file"
        <> metavar "FILE"
        <> action "file" )

legacyToCmd :: LegacyOpt -> Cmd
legacyToCmd LegacyOpt{loPage=pg, loRef=rf, loGrep=gr, loRefs=rs, loGeom=gm,
                      loTagged=tg, loLegacy=lg, loFootnotes=fnn, loRuby=rb,
                      loTitle=tt, loInfo=ii, loToc=oo, loTrailer=tr,
                      loHeight=h, loPassword=pw, loFile=fn} =
  let mpw = maybePassword pw
      mHeight = if null h then Nothing else Just h
      noMode = not gm && not tg && not lg
      plainView = noMode && not fnn && not rb
      textCmd = CmdText (ExtractOpt pg gm tg lg False fnn rb pw fn)
  in case () of
    _ | pg==0 && rf==0 && null gr && not rs && lg && not gm && not tg && not tt && not ii && not oo && not tr ->
        textCmd
      | pg==0 && rf==0 && null gr && not rs && gm && not tg && not lg && not tt && not ii && not oo && not tr ->
        textCmd
      | pg==0 && rf==0 && null gr && not rs && plainView && not tt && not ii && not oo && not tr ->
        CmdView fn mpw mHeight
      | pg==0 && rf==0 && null gr && not rs && (tg || noMode) && not gm && not lg && not tt && not ii && not oo && not tr ->
        textCmd
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
        textCmd
      | pg==0 && null gr && rf/=0 ->
        CmdObject rf fn mpw
      | pg==0 && rf==0 && not (null gr) ->
        CmdGrep gr fn mpw
      | otherwise ->
        textCmd

legacyNeedsDeprecation :: [String] -> Bool
legacyNeedsDeprecation args =
  any (`elem` args)
    [ "-I","--info","-T","--title","-O","--toc","--trailer","-R","--refs","-g","--grep" ]
  || containsFlag args "-r"

containsFlag :: [String] -> String -> Bool
containsFlag [] _ = False
containsFlag (a:as) f = a == f || containsFlag as f
