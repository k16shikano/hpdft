{-# LANGUAGE OverloadedStrings #-}

import PDF.Text (pdfToTextBS, pdfToTextTaggedBS, pdfToTextGeomBSWith)
import PDF.Layout (defaultLayoutOptions)
import PDF.Error (PdfError)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName)
import Control.Monad (when)

fixturesDir, expectedDir, expectedLegacyDir, expectedGeomDir, knownFailingDir :: FilePath
fixturesDir = "data/fixtures"
expectedDir = fixturesDir </> "expected"
expectedLegacyDir = fixturesDir </> "expected-legacy"
expectedGeomDir = fixturesDir </> "expected-geom"
knownFailingDir = fixturesDir </> "known-failing"

main :: IO ()
main = do
  entries <- filter (".pdf" `isSuffixOf`) <$> listDirectory fixturesDir
  let pdfs = map (fixturesDir </>) (sort entries)
  failsDefault <- concat <$> mapM (checkFixture "default" expectedDir pdfToTextTaggedBS) pdfs
  failsLegacy <- concat <$> mapM (checkFixture "legacy" expectedLegacyDir pdfToTextBS) pdfs
  failsGeom <- concat <$> mapM (checkFixture "geom" expectedGeomDir geomExtract) pdfs
  let fails = failsDefault ++ failsLegacy ++ failsGeom
  known <- listKnownFailing
  mapM_ (putStrLn . ("SKIP (known-failing): " ++)) known
  when (not (null fails)) $ do
    mapM_ putStrLn fails
    exitFailure

geomExtract :: FilePath -> Maybe String -> IO (Either PdfError BSL.ByteString)
geomExtract path mpw = pdfToTextGeomBSWith defaultLayoutOptions path mpw

listKnownFailing :: IO [FilePath]
listKnownFailing = do
  exists <- doesDirectoryExist knownFailingDir
  if not exists
    then return []
    else do
      entries <- listDirectory knownFailingDir
      return $ sort $ filter (".pdf" `isSuffixOf`) entries

checkFixture :: String -> FilePath -> (FilePath -> Maybe String -> IO (Either e BSL.ByteString)) -> FilePath -> IO [String]
checkFixture mode expDir extract pdf = do
  let name = takeBaseName pdf
      label = name ++ " (" ++ mode ++ ")"
      expectedPath = expDir </> (name ++ ".txt")
  exists <- doesFileExist expectedPath
  if not exists
    then return [label ++ ": FAIL (missing expected " ++ expectedPath ++ ")"]
    else do
      result <- extract pdf (Just "")
      case result of
        Left _ -> return [label ++ ": FAIL (extraction error)"]
        Right actual -> do
          expected <- BSL.readFile expectedPath
          let actualOut = BSLC.append actual "\n"
          if actualOut == expected
            then do
              putStrLn $ label ++ ": OK"
              return []
            else do
              putStrLn $ label ++ ": FAIL"
              return [diffMessage label actualOut expected]

diffMessage :: String -> BSL.ByteString -> BSL.ByteString -> String
diffMessage name actual expected =
  let aLines = take 20 $ BSLC.lines actual
      eLines = take 20 $ BSLC.lines expected
      pairs = zipWith (\i (a, e) -> show (i + 1) ++ ": -" ++ BSLC.unpack e ++ "\n   +" ++ BSLC.unpack a) [0..] (zip aLines eLines)
  in unlines $ (name ++ " mismatch (first lines):") : pairs
