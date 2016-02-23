{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Data.List 
import Data.Char
import Numeric (readHex)
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Environment (getArgs)

import Debug.Trace

data FFE = Range Int Int | CID Int
         deriving (Show)
                                    
data FFC = Uni Char | Variation [Char] | Dup Char Int | Vert Char | Hw Char | NOTDEF
         deriving (Show)

main :: IO ()
main = do
  (f:fs) <- getArgs
  cont <- readFile f
  let ffmap = tail $ lines cont
  putStrLn $ unlines $ map ((" , "++) . show) $ concatMap (fromFF . expandEntry ffmap) ffmap
  return ()

fromFF :: (FFE, FFC) -> [(Int, T.Text)]
fromFF (Range begin end, Uni code) = zipWith
                                       (\cid unicode -> (cid, textize $ chr unicode))
                                       [begin..end] [(ord code)..]
fromFF (Range _ _, _) = error ""
fromFF (CID cid, Variation vs) = map (\v -> (cid, textize v)) vs
fromFF (CID cid, NOTDEF) = [(cid, T.pack "[NOTDEF]")]
fromFF (CID cid, a) = [(cid, textize $ fromFFC a)]

textize chrcode = T.pack [chrcode]

fromFFC :: FFC -> Char
fromFFC (Uni code) = code
fromFFC (Variation vs) = head vs
fromFFC (Dup code i) = code
fromFFC (Vert code) = code
fromFFC (Hw code) = code


-- | Expand FontForge .cidmap entry
--
-- Examples:
--
-- >>> expandEntry [] "62..63 005d"
-- (Range 62 63,Uni ']')
--
-- >>> expandEntry [] "0 /.notdef"
-- (CID 0,NOTDEF)
--
-- >>> expandEntry [] "1 0020,00a0"
-- (CID 1,Variation "\160 ")
--
-- >>> expandEntry [] "124 /uni2026.dup1"
-- (CID 124,Dup '\8230' 1)
--
-- >>> expandEntry ["646 ff40"] "390 /Japan1.646.hw"
-- (CID 390,Hw '\65344')

expandEntry :: [String] -> String -> (FFE, FFC)
expandEntry ffmap s = let (cids, val) = break (==' ') s
                          ffe = if ".." `isInfixOf` cids
                                then mkrange 
                                     (takeWhile isDigit cids) $  
                                     (dropWhile (=='.') . dropWhile isDigit) cids
                                else CID (read cids :: Int)
                          ffc = readFFValue ffmap $ dropWhile (==' ') val
                      in (ffe, ffc)
  where mkrange s s' = Range (read s :: Int) (read s' :: Int)

readFFValue ffmap s 
  | "/uni" `isPrefixOf` s && "dup1" `isSuffixOf` s = Dup (unicode s) 1
  | "/uni" `isPrefixOf` s && "dup2" `isSuffixOf` s = Dup (unicode s) 2
  | "/uni" `isPrefixOf` s && "vert" `isSuffixOf` s = Vert (unicode s)
  | "/uni" `isPrefixOf` s && "hw" `isSuffixOf` s = Hw (unicode s)
  | "/uni" `isPrefixOf` s = Uni (unicode s) 
  | "/Japan1" `isPrefixOf` s && "vert" `isSuffixOf` s = Vert (jpncode s)
  | "/Japan1" `isPrefixOf` s && "hw" `isSuffixOf` s = Hw (jpncode s)
  | "/Japan1" `isPrefixOf` s = Uni (jpncode s)
  | "," `isInfixOf` s = let (a,b) = break (==',') s
                        in  Variation [hexcode (dropWhile (==',') b), hexcode a]
  | "/.notdef" == s = NOTDEF
  | otherwise = Uni (hexcode s)
  where cidcode :: String -> Char
        cidcode s = case find (s `isPrefixOf`) ffmap of
          Just e -> (fromFFC . snd) $ expandEntry [] e
          Nothing -> ' '  -- error $ "No CID code for "++s
        hexcode = chr . fst . head . readHex
        unicode = hexcode . takeWhile isHexDigit . (\\ "/uni")
        jpncode = cidcode . takeWhile isDigit . (\\ "/Japan1.")
