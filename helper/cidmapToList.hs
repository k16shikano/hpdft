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
      expanded = nub $ concatMap (fromFF . expandEntry ffmap) ffmap 
  putStrLn $ unlines $ map ((" , "++) . show) $ expanded ++ redundantKana expanded
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
  where cidcode s = case find ((s++" ") `isPrefixOf`) ffmap of
          Just e -> (fromFFC . snd) $ expandEntry [] e
          Nothing -> ' '  -- error $ "No CID code for "++s     
        hexcode = chr . fst . head . readHex
        unicode = hexcode . takeWhile isHexDigit . (\\ "/uni")
        jpncode = cidcode . takeWhile isDigit . (\\ "/Japan1.")


fromKana :: [(Int, T.Text)] -> (Int, [Int]) -> [(Int, T.Text)]
fromKana expandedMap (maybeExist, cs) = nub $ map (\c -> (c, extraKana expandedMap maybeExist)) cs

extraKana emap v = case lookup v emap of
  Just c -> c
  Nothing -> "????"

redundantKana expandedMap = concatMap (fromKana expandedMap)
-- exerpt from https://gist.github.com/zr-tex8r/527b977dec4165934d31
  [ (651, [651,12362,12545,12649,12649])
  , (652, [652,12363,12546,12650,12650])
  , (653, [653,12273,12456,12651,12651])
  , (654, [654,12274,12457,12652,12652])
  , (660, [7891,12364,12547,12867,12868])
  , (842, [7918,12275,12458,12671,12757])
  , (843, [843,12276,12459,12672,12672])
  , (844, [7919,12277,12460,12673,12758])
  , (845, [845,12278,12461,12674,12674])
  , (846, [7920,12279,12462,12675,12759])
  , (847, [847,12280,12463,12676,12676])
  , (848, [7921,12281,12464,12677,12760])
  , (849, [849,12282,12465,12678,12678])
  , (850, [7922,12283,12466,12679,12761])
  , (851, [851,12284,12467,12680,12680])
  , (852, [852,12286,12469,12681,12681])
  , (853, [853,12287,12470,12683,12683])
  , (854, [854,12288,12471,12684,12684])
  , (855, [855,12289,12472,12685,12685])
  , (856, [856,12290,12473,12686,12686])
  , (857, [857,12291,12474,12687,12687])
  , (858, [858,12293,12476,12688,12688])
  , (859, [859,12294,12477,12690,12690])
  , (860, [860,12296,12479,12691,12691])
  , (861, [861,12297,12480,12692,12692])
  , (862, [862,12298,12481,12693,12693])
  , (863, [863,12299,12482,12694,12694])
  , (864, [864,12300,12483,12695,12695])
  , (865, [865,12301,12484,12696,12696])
  , (866, [866,12302,12485,12697,12697])
  , (867, [867,12303,12486,12698,12698])
  , (868, [868,12304,12487,12699,12699])
  , (869, [869,12305,12488,12700,12700])
  , (870, [870,12306,12489,12701,12701])
  , (871, [871,12307,12490,12702,12702])
  , (872, [872,12308,12491,12703,12703])
  , (873, [873,12309,12492,12704,12704])
  , (874, [874,12310,12493,12705,12705])
  , (875, [875,12311,12494,12706,12706])
  , (876, [7923,12312,12495,12707,12764])
  , (877, [877,12313,12496,12708,12708])
  , (878, [878,12314,12497,12709,12709])
  , (879, [879,12315,12498,12710,12710])
  , (880, [880,12316,12499,12711,12711])
  , (881, [881,12317,12500,12712,12712])
  , (882, [882,12318,12501,12713,12713])
  , (883, [883,12319,12502,12714,12714])
  , (884, [884,12320,12503,12715,12715])
  , (885, [885,12321,12504,12716,12716])
  , (886, [886,12322,12505,12717,12717])
  , (887, [887,12323,12506,12718,12718])
  , (888, [888,12324,12507,12719,12719])
  , (889, [889,12325,12508,12720,12720])
  , (890, [890,12326,12509,12721,12721])
  , (891, [891,12327,12510,12722,12722])
  , (892, [892,12328,12511,12723,12723])
  , (893, [893,12329,12512,12724,12724])
  , (894, [894,12330,12513,12725,12725])
  , (895, [895,12331,12514,12726,12726])
  , (896, [896,12332,12515,12727,12727])
  , (897, [897,12333,12516,12728,12728])
  , (898, [898,12334,12517,12729,12729])
  , (899, [899,12335,12518,12730,12730])
  , (900, [900,12336,12519,12731,12731])
  , (901, [901,12337,12520,12732,12732])
  , (902, [902,12338,12521,12733,12733])
  , (903, [903,12339,12522,12734,12734])
  , (904, [904,12340,12523,12735,12735])
  , (905, [905,12341,12524,12736,12736])
  , (906, [906,12342,12525,12737,12737])
  , (907, [907,12343,12526,12738,12738])
  , (908, [7924,12344,12527,12739,12765])
  , (909, [909,12345,12528,12740,12740])
  , (910, [7925,12346,12529,12741,12766])
  , (911, [911,12347,12530,12742,12742])
  , (912, [7926,12348,12531,12743,12767])
  , (913, [913,12349,12532,12744,12744])
  , (914, [914,12350,12533,12745,12745])
  , (915, [915,12351,12534,12746,12746])
  , (916, [916,12352,12535,12747,12747])
  , (917, [917,12353,12536,12748,12748])
  , (918, [918,12354,12537,12749,12749])
  , (919, [7927,12355,12538,12750,12768])
  , (920, [920,12356,12539,12751,12751])
  , (921, [921,12357,12540,12752,12752])
  , (922, [922,12358,12541,12753,12753])
  , (923, [923,12359,12542,12754,12754])
  , (924, [924,12360,12543,12755,12755])
  , (7958, [7958,12361,12544,12756,12756])
  , (7959, [8264,12285,12468,12682,12762])
  , (7960, [8265,12292,12475,12689,12763])
  , (16209, [16209,16352,16382,16414,16414])
  , (16210, [16210,16353,16383,16415,16415])
  , (16211, [16211,16354,16384,16416,16416])
  , (16212, [16212,16355,16385,16417,16417])
  , (16213, [16213,16356,16386,16418,16418])
  , (925, [7928,12365,12548,12769,12855])
  , (926, [926,12366,12549,12770,12770])
  , (927, [7929,12367,12550,12771,12856])
  , (928, [928,12368,12551,12772,12772])
  , (929, [7930,12369,12552,12773,12857])
  , (930, [930,12370,12553,12774,12774])
  , (931, [7931,12371,12554,12775,12858])
  , (932, [932,12372,12555,12776,12776])
  , (933, [7932,12373,12556,12777,12859])
  , (934, [934,12374,12557,12778,12778])
  , (935, [935,12376,12559,12779,12779])
  , (936, [936,12377,12560,12781,12781])
  , (937, [937,12378,12561,12782,12782])
  , (938, [938,12379,12562,12783,12783])
  , (939, [939,12380,12563,12784,12784])
  , (940, [940,12381,12564,12785,12785])
  , (941, [941,12383,12566,12786,12786])
  , (942, [942,12384,12567,12788,12788])
  , (943, [943,12386,12569,12789,12789])
  , (944, [944,12387,12570,12790,12790])
  , (945, [945,12388,12571,12791,12791])
  , (946, [946,12389,12572,12792,12792])
  , (947, [947,12390,12573,12793,12793])
  , (948, [948,12391,12574,12794,12794])
  , (949, [949,12392,12575,12795,12795])
  , (950, [950,12393,12576,12796,12796])
  , (951, [951,12394,12577,12797,12797])
  , (952, [952,12395,12578,12798,12798])
  , (953, [953,12396,12579,12799,12799])
  , (954, [954,12397,12580,12800,12800])
  , (955, [955,12398,12581,12801,12801])
  , (956, [956,12399,12582,12802,12802])
  , (957, [957,12400,12583,12803,12803])
  , (958, [958,12401,12584,12804,12804])
  , (959, [7933,12402,12585,12805,12862])
  , (960, [960,12403,12586,12806,12806])
  , (961, [961,12404,12587,12807,12807])
  , (962, [962,12405,12588,12808,12808])
  , (963, [963,12406,12589,12809,12809])
  , (964, [964,12407,12590,12810,12810])
  , (965, [965,12408,12591,12811,12811])
  , (966, [966,12409,12592,12812,12812])
  , (967, [967,12410,12593,12813,12813])
  , (968, [968,12411,12594,12814,12814])
  , (969, [969,12412,12595,12815,12815])
  , (970, [970,12413,12596,12816,12816])
  , (971, [971,12414,12597,12817,12817])
  , (972, [972,12415,12598,12818,12818])
  , (973, [973,12416,12599,12819,12819])
  , (974, [974,12417,12600,12820,12820])
  , (975, [975,12418,12601,12821,12821])
  , (976, [976,12419,12602,12822,12822])
  , (977, [977,12420,12603,12823,12823])
  , (978, [978,12421,12604,12824,12824])
  , (979, [979,12422,12605,12825,12825])
  , (980, [980,12423,12606,12826,12826])
  , (981, [981,12424,12607,12827,12827])
  , (982, [982,12425,12608,12828,12828])
  , (983, [983,12426,12609,12829,12829])
  , (984, [984,12427,12610,12830,12830])
  , (985, [985,12428,12611,12831,12831])
  , (986, [986,12429,12612,12832,12832])
  , (987, [987,12430,12613,12833,12833])
  , (988, [988,12431,12614,12834,12834])
  , (989, [989,12432,12615,12835,12835])
  , (990, [990,12433,12616,12836,12836])
  , (991, [7934,12434,12617,12837,12863])
  , (992, [992,12435,12618,12838,12838])
  , (993, [7935,12436,12619,12839,12864])
  , (994, [994,12437,12620,12840,12840])
  , (995, [7936,12438,12621,12841,12865])
  , (996, [996,12439,12622,12842,12842])
  , (997, [997,12440,12623,12843,12843])
  , (998, [998,12441,12624,12844,12844])
  , (999, [999,12442,12625,12845,12845])
  , (1000, [1000,12443,12626,12846,12846])
  , (1001, [1001,12444,12627,12847,12847])
  , (1002, [7937,12445,12628,12848,12866])
  , (1003, [1003,12446,12629,12849,12849])
  , (1004, [1004,12447,12630,12850,12850])
  , (1005, [1005,12448,12631,12851,12851])
  , (1006, [1006,12449,12632,12852,12852])
  , (1007, [1007,12450,12633,12853,12853])
  , (1008, [1008,12451,12634,12854,12854])
  , (1009, [7938,12375,12558,12780,12860])
  , (1010, [7939,12382,12565,12787,12861])
  , (16214, [16214,16357,16387,16419,16419])
  , (16215, [16215,16358,16388,16420,16420])
  , (16216, [16216,16359,16389,16421,16421])
  , (16217, [16217,16360,16390,16422,16422])
  , (16218, [16218,16361,16391,16423,16423])
  , (16219, [16219,16362,16392,16424,16424])
  , (16220, [16220,16363,16393,16425,16425])
  , (16221, [16221,16364,16394,16426,16426])
  , (16236, [16333,16365,16395,16427,16450])
  , (16237, [16334,16366,16396,16428,16451])
  , (16238, [16335,16367,16397,16429,16452])
  , (16239, [16336,16368,16398,16430,16453])
  , (16240, [16337,16369,16399,16431,16454])
  , (16241, [16338,16370,16400,16432,16455])
  , (16242, [16339,16371,16401,16433,16456])
  , (16243, [16340,16372,16402,16434,16457])
  , (16244, [16341,16373,16403,16435,16458])
  , (16245, [16342,16374,16404,16436,16459])
  , (16246, [16343,16375,16405,16437,16460])
  , (16247, [16344,16376,16406,16438,16461])
  , (16248, [16345,16377,16407,16439,16462])
  , (16249, [16346,16378,16408,16440,16463])
  , (16250, [16347,16379,16409,16441,16464])
  , (16251, [16348,16380,16410,16442,16465])
  , (16252, [16349,16381,16411,16443,16466])
  , (8313, [8313,12452,12635,16444,16444])
  , (8314, [8314,12453,12636,16445,16445])
  , (8315, [8315,12454,12637,16446,16446])
  , (8316, [8316,12455,12638,16447,16447])
  ]