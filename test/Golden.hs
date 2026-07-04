{-# LANGUAGE OverloadedStrings #-}

import PDF.Text (pdfToTextBS)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (isSuffixOf, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName)
import Control.Monad (when)

fixturesDir, expectedDir, knownFailingDir :: FilePath
fixturesDir = "data/fixtures"
expectedDir = fixturesDir </> "expected"
knownFailingDir = fixturesDir </> "known-failing"

main :: IO ()
main = do
  entries <- filter (".pdf" `isSuffixOf`) <$> listDirectory fixturesDir
  let pdfs = map (fixturesDir </>) (sort entries)
  fails <- concat <$> mapM checkFixture pdfs
  known <- listKnownFailing
  mapM_ (putStrLn . ("SKIP (known-failing): " ++)) known
  when (not (null fails)) $ do
    mapM_ putStrLn fails
    exitFailure

listKnownFailing :: IO [FilePath]
listKnownFailing = do
  exists <- doesDirectoryExist knownFailingDir
  if not exists
    then return []
    else do
      entries <- listDirectory knownFailingDir
      return $ sort $ filter (".pdf" `isSuffixOf`) entries

checkFixture :: FilePath -> IO [String]
checkFixture pdf = do
  let name = takeBaseName pdf
      expectedPath = expectedDir </> (name ++ ".txt")
  exists <- doesFileExist expectedPath
  if not exists
    then return [name ++ ": FAIL (missing expected " ++ expectedPath ++ ")"]
    else do
      actual <- pdfToTextBS pdf (Just "")
      expected <- BSL.readFile expectedPath
      let actualOut = BSLC.append actual "\n"
      if actualOut == expected
        then do
          putStrLn $ name ++ ": OK"
          return []
        else do
          putStrLn $ name ++ ": FAIL"
          return [diffMessage name actualOut expected]

diffMessage :: String -> BSL.ByteString -> BSL.ByteString -> String
diffMessage name actual expected =
  let aLines = take 20 $ BSLC.lines actual
      eLines = take 20 $ BSLC.lines expected
      pairs = zipWith (\i (a, e) -> show (i + 1) ++ ": -" ++ BSLC.unpack e ++ "\n   +" ++ BSLC.unpack a) [0..] (zip aLines eLines)
  in unlines $ (name ++ " mismatch (first lines):") : pairs
