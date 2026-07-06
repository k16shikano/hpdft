module Cli.Parser
  ( Cmd(..)
  , ExtractOpt(..)
  , ImagesOpt(..)
  , FormOpt(..)
  , DiffOpt(..)
  , LegacyOpt(..)
  ) where

data Cmd
  = CmdText ExtractOpt
  | CmdView FilePath (Maybe String) (Maybe String)
  | CmdImage ImagesOpt
  | CmdForm FormOpt
  | CmdDiff DiffOpt
  | CmdInfo FilePath (Maybe String)
  | CmdTitle FilePath (Maybe String)
  | CmdToc FilePath (Maybe String)
  | CmdTrailer FilePath
  | CmdObject Int FilePath (Maybe String)
  | CmdRefs FilePath (Maybe String)
  | CmdGrep String FilePath (Maybe String)

data ExtractOpt = ExtractOpt
  { eoPage      :: Int
  , eoGeom      :: Bool
  , eoTagged    :: Bool
  , eoLegacy    :: Bool
  , eoQuiet     :: Bool
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

data FormOpt = FormOpt
  { foPage     :: Int
  , foName     :: Maybe String
  , foOut      :: FilePath
  , foPassword :: String
  , foFile     :: FilePath
  }

data DiffOpt = DiffOpt
  { doGeom      :: Bool
  , doRuby      :: Bool
  , doJson      :: Bool
  , doPassword  :: String
  , doFileA     :: FilePath
  , doFileB     :: FilePath
  }

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
  , loHeight    :: String
  , loPassword  :: String
  , loFile      :: FilePath
  }
