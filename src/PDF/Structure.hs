{-# LANGUAGE OverloadedStrings #-}

module PDF.Structure
  ( StructElem(..)
  , StructKid(..)
  , RubySpan(..)
  , structTree
  , logicalOrder
  , collectRubySpans
  ) where

import PDF.Definition
import PDF.Document (Document(..), docRootRef)
import PDF.DocumentStructure
  ( findDict
  , findDictByRef
  , findDictOfType
  , findObjFromDict
  , findObjsByRef
  )
import PDF.Error (PdfError(..), PdfResult)

import qualified Data.Text as T
import Data.List (foldl')
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as S

data StructElem = StructElem
  { seType :: T.Text
  , seKids :: [StructKid]
  } deriving (Eq, Show)

data StructKid = KidElem StructElem | KidMCID Int Int deriving (Eq, Show)

-- Parallel base/ruby MCID lists from a /Ruby element's /RB and /RT kids.
data RubySpan = RubySpan
  { rsPage   :: Int
  , rsBases  :: [Int]
  , rsRubies :: [Int]
  } deriving (Eq, Show)

maxStructDepth :: Int
maxStructDepth = 512

structTree :: Document -> PdfResult (Maybe StructElem)
structTree doc = do
  rootRef <- docRootRef doc
  let objs = docObjs doc
  case findObjsByRef rootRef objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just catalog -> case findObjFromDict catalog "/StructTreeRoot" of
        Just (ObjRef r) -> parseStructRef r Nothing objs S.empty 0
        Just (PdfDict d) -> parseStructDict d Nothing objs S.empty 0
        _ -> Right Nothing
      Nothing -> Left (MissingKey "/Type" "catalog")
    Nothing -> Left (MissingObject rootRef)

parseStructRef :: Int -> Maybe Int -> PDFObjIndex -> S.Set Int -> Int
             -> PdfResult (Maybe StructElem)
parseStructRef ref pg objs visited depth
  | depth >= maxStructDepth = Right Nothing
  | S.member ref visited = Right Nothing
  | otherwise =
      case findObjsByRef ref objs of
        Just os -> case findDict os of
          Just d -> parseStructDict d pg objs (S.insert ref visited) depth
          Nothing -> Right Nothing
        Nothing -> Left (MissingObject ref)

parseStructDict :: Dict -> Maybe Int -> PDFObjIndex -> S.Set Int -> Int
                -> PdfResult (Maybe StructElem)
parseStructDict d pg objs visited depth = do
  let pg' = pageRefFromDict d pg objs
      stype = structTypeName d
  kids <- parseKids (findObjFromDict d "/K") pg' objs visited (depth + 1)
  if T.null stype && null kids
     then return Nothing
     else return (Just (StructElem stype kids))

structTypeName :: Dict -> T.Text
structTypeName d =
  case findObjFromDict d "/S" of
    Just (PdfName n) -> n
    _ -> case findObjFromDict d "/Type" of
      Just (PdfName n) -> n
      _ -> T.empty

logicalOrder :: StructElem -> [([T.Text], Int, Int)]
logicalOrder root = walk [] root
  where
    walk ancestors (StructElem stype kids) =
      let path = ancestors ++ [stype]
      in concatMap (kidWalk path) kids

    kidWalk path (KidMCID page mcid) = [(path, page, mcid)]
    kidWalk path (KidElem elem) = walk path elem

collectRubySpans :: StructElem -> [RubySpan]
collectRubySpans root = walk root
  where
    walk (StructElem stype kids) =
      let childSpans = concatMap kidWalk kids
          here = if stype == "/Ruby" then maybeToList (rubySpan kids) else []
      in here ++ childSpans

    kidWalk (KidElem e) = walk e
    kidWalk _ = []

rubySpan :: [StructKid] -> Maybe RubySpan
rubySpan kids =
  case (findKidElem "/RB" kids, findKidElem "/RT" kids) of
    (Just rb, Just rt) ->
      let bases = mcidsFromElem rb
          rubies = mcidsFromElem rt
      in case bases of
        (page, _) : _ ->
          if null bases || null rubies
             then Nothing
             else Just (RubySpan page (map snd bases) (map snd rubies))
        [] -> Nothing
    _ -> Nothing

findKidElem :: T.Text -> [StructKid] -> Maybe StructElem
findKidElem want = foldr go Nothing
  where
    go (KidElem e@(StructElem t _)) Nothing
      | t == want = Just e
    go _ acc = acc

mcidsFromElem :: StructElem -> [(Int, Int)]
mcidsFromElem (StructElem _ kids) = concatMap kidMcids kids
  where
    kidMcids (KidMCID page mcid) = [(page, mcid)]
    kidMcids (KidElem e) = mcidsFromElem e
    kidMcids _ = []

pageRefFromDict :: Dict -> Maybe Int -> PDFObjIndex -> Maybe Int
pageRefFromDict d pg _ =
  case findObjFromDict d "/Pg" of
    Just (ObjRef r) -> Just r
    _               -> pg

parseKids :: Maybe Obj -> Maybe Int -> PDFObjIndex -> S.Set Int -> Int
          -> PdfResult [StructKid]
parseKids Nothing _ _ _ _ = Right []
parseKids (Just (PdfNumber n)) pg _ _ _ =
  case pg of
    Just p -> Right [KidMCID p (truncate n)]
    Nothing -> Right []
parseKids (Just (PdfArray arr)) pg objs visited depth =
  foldl' (\acc o -> do
    ks <- acc
    more <- parseKid o pg objs visited depth
    return (ks ++ more)) (Right []) arr
parseKids (Just o) pg objs visited depth = parseKid o pg objs visited depth

parseKid :: Obj -> Maybe Int -> PDFObjIndex -> S.Set Int -> Int
         -> PdfResult [StructKid]
parseKid (PdfNumber n) pg _ _ _ =
  case pg of
    Just p -> Right [KidMCID p (truncate n)]
    Nothing -> Right []
parseKid (ObjRef r) pg objs visited depth = do
  case findObjsByRef r objs of
    Just os -> case findDict os of
      Just d -> parseKidDict d pg objs visited depth
      Nothing -> Right []
    Nothing -> Left (MissingObject r)
parseKid (PdfDict d) pg objs visited depth = parseKidDict d pg objs visited depth
parseKid _ _ _ _ _ = Right []

parseKidDict :: Dict -> Maybe Int -> PDFObjIndex -> S.Set Int -> Int
             -> PdfResult [StructKid]
parseKidDict d pg objs visited depth =
  case findObjFromDict d "/Type" of
    Just (PdfName "/MCR") ->
      let pg' = pageRefFromDict d pg objs
          mcid = case findObjFromDict d "/MCID" of
            Just (PdfNumber n) -> Just (truncate n)
            _                  -> Nothing
      in case (pg', mcid) of
        (Just p, Just m) -> Right [KidMCID p m]
        _                -> Right []
    Just (PdfName "/OBJR") -> Right []
    _ -> do
      elem' <- parseStructDict d pg objs visited depth
      case elem' of
        Just e  -> Right [KidElem e]
        Nothing -> Right []
