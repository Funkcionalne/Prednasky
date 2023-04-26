module Sudoku where

import Data.List 

type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Value            = Char



boxsize               :: Int
boxsize               =  3
--boxsize               = 4
--boxsize               = 5

values                :: [Value]
values | boxsize == 3 = "123456789"
       | boxsize == 4 = "123456789abcdefh"
       | boxsize == 5 = "ABCDEFGHIJKLMNOPQRSTUVWXY"

empty                 :: Value -> Bool
empty                 =  (== '.')

single                :: [a] ->Bool
single [_]            =  True
single _              =  False

-- (0.03 secs, 0 bytes)
easy                  :: Grid
easy                  =  ["2....1.38",
                          "........5",
                          ".7...6...",
                          ".......13",
                          ".981..257",
                          "31....8..",
                          "9..8...2.",
                          ".5..69784",
                          "4..25...."]

-- (0.05 secs, 0 bytes)
gentle                :: Grid
gentle                =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]

-- (0.09 secs, 13,351,800 bytes)
diabolical            :: Grid
diabolical            =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]

-- (0.09 secs, 15,082,328 bytes)
unsolvable            :: Grid
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4.",
                          "2..8.6..9"]

-- (9.00 secs, 4,367,832,776 bytes)
minimal               :: Grid
minimal               =  [".98......",
                          "....7....",
                          "....15...",
                          "1........",
                          "...2....9",
                          "...9.6.82",
                          ".......3.",
                          "5.1......",
                          "...4...2."]

new1               =  [                          
                          "6.......3",
                          "8..4561..",
                          ".5.......",
                          ".159..3..",
                          "....1....",
                          ".6..8.5.7",
                          "..2......",
                          "9....174.",
                          "47..9...6"]                         
                          
new2               =  [                          
                          "...4....1",
                          ".4.2..5..",
                          ".79....3.",
                          ".........",
                          "6..58.1.7",
                          ".8.6.....",
                          ".5...73..",
                          "26.....4.",
                          "..8..4..."]
                          
blank                 :: Grid
blank                 =  replicate n (replicate n '.')
                         where n = boxsize ^ 2



rows                  :: Matrix a -> [Row a]
rows                  =  id


cols                  :: Matrix a -> [Row a]
cols                  =  transpose'

transpose'             :: Matrix a -> Matrix a
transpose' [xs]        = [[x] | x <- xs]
transpose' (xs:xss)     = zipWith (:) xs (transpose' xss)

transpose''        :: Matrix a -> Matrix a
transpose'' xss    = foldr (\xs -> \rek  -> zipWith (:) xs rek) 
                      (replicate (length xss) []) 
                      -- [ [] | _ <- [1..(length xss)]]
                      -- (replicate 1000 [])
                      xss

transpose'''             :: Matrix a -> Matrix a
transpose''' (xs:xss)    = foldl (\acc -> \ys -> zipWith (:) ys acc) [[x] | x <- xs] xss

transpose''''             :: Matrix a -> Matrix a
transpose'''' (xs:xss)    = foldl (\acc -> \ys -> zipWith (\x -> \y -> x++[y]) acc ys) [[x] | x <- xs] xss


boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                         where
                            unpack = map concat . concat
                            pack   = group3 . map group3
                            group3  = group boxsize
                            group         :: Int -> [a] -> [[a]]
                            group n []    =  []
                            group n xs    =  take n xs : group n (drop n xs)


valid                 :: Grid -> Bool
valid g               =  all nodups (rows g) &&
                         all nodups (cols g) &&
                         all nodups (boxs g)

nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs



type Choices          =  [Value]

choices               :: Grid -> Matrix Choices
choices               =  map (map choice)
                         where
                            choice v = if empty v then values else [v]

cp                    :: [[a]] -> [[a]]
cp []                 =  [[]]
cp (xs:xss)           =  [y:ys | y <- xs, ys <- cp xss]


expand              :: Matrix [a] -> [Matrix a]
expand              =  cp . map cp

solver                 :: Grid -> [Grid]
solver                 =  filter valid . expand . choices


prune                 :: Matrix Choices -> Matrix Choices
prune                 =  pruneBy boxs . pruneBy cols . pruneBy rows
                         where pruneBy f = f . map reduce . f

reduce                :: Row Choices -> Row Choices
reduce xss            =  [xs `minus` singles | xs <- xss]
                         where singles = concat (filter single xss)

minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs \\ ys

solver2                :: Grid -> [Grid]
solver2                =  filter valid . expand . prune . choices


solver3                :: Grid -> [Grid]
solver3                =  filter valid . expand . fix prune . choices

fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                         where x' = f x

complete              :: Matrix Choices -> Bool
complete              =  all (all single)


void                  :: Matrix Choices -> Bool
void                  =  any (any null)

safe                  :: Matrix Choices -> Bool
safe cm               =  all consistent (rows cm) &&
                         all consistent (cols cm) &&
                         all consistent (boxs cm)

consistent            :: Row Choices -> Bool
consistent            =  nodups . concat . filter single


blocked               :: Matrix Choices -> Bool
blocked m             =  void m || not (safe m)

solver4                :: Grid -> [Grid]
solver4                =  search . prune . choices

search                :: Matrix Choices -> [Grid]
search m
 | blocked m          =  []
 | complete m         =  expand m
 | otherwise          =  [g | m' <- expand_ m
                            , g  <- search (prune m')]

expand_                :: Matrix Choices -> [Matrix Choices]
expand_ m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any (not . single)) m
      (row1,cs:row2)    = break (not . single) row

------ DU
-- uloha expandMin
expandMin             :: Matrix Choices -> [Matrix Choices]
expandMin m           = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      minLength = foldr (\row min -> (foldr (\cell min' -> let cl = length cell in (if cl > 1 then (if cl < min' then cl else min') else min')) min row)) 9 m
      isLengthOf a = length a == minLength
      (rows1,row:rows2) = break (any (isLengthOf)) m
      (row1,cs:row2)    = break (isLengthOf) row

search'               :: Matrix Choices -> [Grid]
search' m
 | blocked m          =  []
 | complete m         =  expand m
 | otherwise          =  [g | m' <- expandMin m
                            , g  <- search' (fix prune m')]

solver5                :: Grid -> [Grid]
solver5                =  search' . prune . choices
---------------------------------  
--------------------- upravene sudoku, autor M.W.

solver6 = mysearch1 . choices
safe1 m = all ok (rows m) && all ok (cols m) && all ok (boxs m)

ok row = nodups [d | [d] <- row]  

myprune :: Matrix Choices -> Matrix Choices
myprune = mypruneBy boxs . mypruneBy cols . mypruneBy rows

mypruneBy :: (Matrix Choices -> Matrix Choices) -> (Matrix Choices -> Matrix Choices)
mypruneBy f = f . map xreduce . map reduce . f

mysearch1 :: Matrix Choices -> Matrix Choices
mysearch1 m 
  | not (safe1 m1) = []
  | complete m1    = [map (map head) m1]
  | otherwise      = concat (map mysearch1 (expand_ m1))
  where m1 = fup m

  
-- xreduce je jednoducha optimalizacia, ktoru poznaju aj stare babky
-- ak sa v riadku/stlplci uz nachadza jedina moznost napr. pre x=3, musi tam byt 3
xreduce :: Row Choices -> Row Choices 
xreduce r = map unique r
            where
               unique :: Choices -> Choices 
               unique e    | single e   = e    -- ak je tam jedina moznost - nerobi nic e->e
                  | null r'     = e   -- ziadna z e sa nenachadza v jedinom policku - nerobi nic e->e
                  | otherwise  = r'  -- r' = [x], takze x sa nachadza ako jedinecna v r,
                                      -- jedine tu sa nieco zmeni, ak x sa nachadza prave v jednom policku riadku/stplca
                      where r'    = check e r  -- vtedy zredukuje e choces [x]


-- hlada tu choice, ktora sa nachadza v prave jednom policku riadku/stlpca/...                  
-- vrati jednoprvkovy zoznam, alebo nil ak taka nie je
check :: Choices -> Row Choices -> Choices
check [] r      = []
check (x:xs) r   | single (filter (elem x) r) = [x]  -- ak sa x nachadza v jednom policku riadku, tak [x]
                 | otherwise                  = check xs r

-- fixpoint cez myprune = opakuj, kym je co rezat 
--fup   :: Matrix Choices -> Matrix Choices
fup m | pm /= m   = fup pm
      | otherwise = m
      where pm = myprune m  

---------------------------------


main                  :: IO ()
main                  =  putStrLn $ unlines $ head $ solver4
                          -- blank
                          -- gentle
                          diabolical

doit g solver               =  putStrLn $ unlines $ head $ solver g
                          
e = [[9*i+j+1 | j <- [0..8]] | i <- [0..8]]

--- prevzate od Misa W.

-- (0.03 secs, 0 bytes) - lahke
m2 = [".52769..8",
      "76.8.3.9.",
      ".83.4....",
      "..4......",
      ".2961487.",
      "......5..",
      "....7.231",
      ".4.5.2.87",
      "2..19864."]
    
-- (0.05 secs, 0 bytes)
m3 = ["86..9...5", --108
      ".5.8.....",
      "73.1..8..",
      "..54...7.",
      ".........",
      ".8...36..",
      "..2..5.19",
      ".....9.2.",
      "3...1..56"]

-- (0.08 secs, 14,933,544 bytes)      
m4 = ["263.1.5..", --101
      "...54.2..",
      ".....2...",
      "....91..2",
      ".46...81.",
      "7..48....",
      "...9.....",
      "..1.34...",
      "..8.2.469"]

-- (0.02 secs, 0 bytes)
m5 = ["17.......",
      ".85.293..",
      "3..8..257",
      "634..8592",
      ".........",
      "7594..138",
      "921..5..3",
      "..394.82.",
      ".......65"]

-- (0.27 secs, 103,953,528 bytes)     
m6 = ["6....4...",   --tazke fup nevyriesi
      "..8..7...",
      "71..8.9..",
      ".3...57.1",
      ".8.....2.",
      "5.79...6.",
      "..1.6..72",
      "...4..5..",
      "...5....9"]
      
-- (0.09 secs, 15,129,496 bytes)	  
m7 = ["...7.6.8.",
      "..6.59.13",
      ".......9.",
      "95....12.",
      "....7....",
      ".61....39",
      ".3.......",
      "24.59.3..",
      ".8.1.2..."]  

-- size 4x4      
h1 = ["....h.4....a.8.1",
      "31.485.b67.....d",
      "..e..d...2..6..a",
      ".dca..19........",
      "..5..b..h.....8.",
      ".8.c....9.7.2...",
      "a7..c3h.........",
      "f.h62.81c.....75",
      "5.4.......1eh9a.",
      "..6.1a.h.9..c.d.",
      "b.f.9e...c.38...",
      ".a..d..2...6.3.f",
      ".....4.e.82h3b..",
      "6..2..f.3.5.d...",
      "..df.h.5...71.6.",
      "9.1b.8...4.d..h."]
{-      
"?: " doit h1 solver5
76b5h24fd3ea98c1
319485ab67hcef2d
hfe87dc3429165ba
2dcae6195fb87h34
d95e4b76h1a2fc83
483c5fea967b2d1h
a721c3hde58f469b
fbh62981cd34ae75
5c4df7382b1eh9a6
8e631abh79f5c4d2
b2fh9e64acd38157
1a79dc528h46b3ef
c5a764de182h3bf9
6h82b1f73e59da4c
e4df3h95bac71268
931ba82cf46d57he
(595.02 secs, 363,070,515,264 bytes)
(662.27 secs, 363,082,842,168 bytes)

*Sudoku> doit h1 solver4
(403.61 secs, 199,446,724,008 bytes)

"?: " doit h1 solver6
(0.06 secs, 25,777,440 bytes)
-}

      
h2 = ["....1.5....b.9.2",
      "42.596.c78.....e",
      "..f..e...3..7..b",
      ".edb..2a........",
      "6.5.......2f1ab.",
      "..7.2b.1.a..d.e.",
      "c.h.af...d.49...",
      ".b..e..3...7.4.h",
      "..6..c..1.....9.",
      ".9.d....a.8.3...",
      "b8..d41.........",
      "h.173.92d.....86",
      ".....5.f.9314c..",
      "7..3..h.4.6.e...",
      "..eh.1.6...82.7.",
      "a.2c.9...5.e..1."]    
{-
"?: " doit h2 solver5
87c6135he4fba9d2
42a596bc781dfh3e
1hf98ed453a276cb
3edbf72a6hc98145
6d5eh8493c2f1ab7
9f742bc18ah6d5e3
c3h1af75bde49268
2b8aed639157c4fh
ea6f5c8712b3hd94
594d6hfba78c3e21
b832d41ef69h57ac
hc173a92de45bf86
d6b875ef29314cha
7193c2h84f6aeb5d
f5eh41a6cbd82379
a42cb93dh57e681f
(396.87 secs, 241,018,104,896 bytes)

"?: " doit h2 solver6
87c6135he4fba9d2
42a596bc781dfh3e
1hf98ed453a276cb
3edbf72a6hc98145
6d5eh8493c2f1ab7
9f742bc18ah6d5e3
c3h1af75bde49268
2b8aed639157c4fh
ea6f5c8712b3hd94
594d6hfba78c3e21
b832d41ef69h57ac
hc173a92de45bf86
d6b875ef29314cha
7193c2h84f6aeb5d
f5eh41a6cbd82379
a42cb93dh57e681f
(0.05 secs, 14,646,936 bytes)

-}      
  
xh1 =["..UYR.Q.E..T.V..B.N.SIC..",
      "..LXO...BC.IWQ.VG...KTJ..",
	  "GEB.Q..JMAH...PITD..O.NWV",
	  "CA.....XT.NF.MO.KH.....YL",
	  "IJT..UK.P.GS.LB.O.YE..HFQ",
	  "....VS...X.L.P.J...GH....",
	  "D...P.IBU.KO.NA.SCF.Q...J",
	  "..FO..H..LUWYECR..D..VS..",
	  "EYQUJ.P.CN.....XW.O.LGBAF",
	  ".CM..F.TG..B.J..AQ.V..PU.",
	  "..SDK.JO.........VM.RQX..",
	  "TF.MXRSN.E.....B.GIJYP.VU",
	  ".H.....L.........N.....O.",
	  "RL.BIXUY.V.....D.OKPEC.HG",
	  "..VEU.CG.........ST.MLI..",
	  ".DN..J.WL..C.A..UE.O..YM.",
	  "LSRFA.E.NO.....CM.Q.UKVPH",
	  "..OG..D..BVYHKLF..R..AQ..",
	  "V...T.GUF.WM.OE.XPA.I...R",
	  "....BA...Q.N.S.T...LJ....",
	  "JVC..EB.O.QR.FT.D.XU..MSY",
	  "QB.....PA.JX.IH.YK.....LD",
	  "STH.L..QRYO...UMEA..C.FKB",
	  "..DNF...VK.EAY.OJ...PHG..",
	  "..IAY.L.S..K.B..Q.V.WRU.."]
{-
"?: " doit xh1 solver6
HKUYROQFEDATJVXWBLNMSICGP
FMLXOHNSBCYIWQDVGUPRKTJEA
GEBSQLYJMAHUKRPITDCFOXNWV
CAPVDGWXTINFEMOSKHJQBURYL
IJTWNUKVPRGSCLBAOXYEDMHFQ
AIWTVSOEYXFLMPQJNBUGHDKRC
DRXHPVIBUMKOGNALSCFYQWETJ
BNFOGQHAJLUWYECRPTDKXVSIM
EYQUJDPKCNRVTHSXWMOILGBAF
KCMLSFRTGWDBXJIEAQHVNYPUO
YPSDKTJOIHEAFCGULVMWRQXBN
TFAMXRSNQELHODKBCGIJYPWVU
WHGQCMALKPBJIUVYRNEXFSDOT
RLJBIXUYWVMQSTNDFOKPECAHG
NOVEUBCGDFXPRWYQHSTAMLIJK
XDNIHJVWLTPCQARKUEBOGFYMS
LSRFAYEINOTGBXJCMWQDUKVPH
MUOGEPDCXBVYHKLFIJRSTAQNW
VQYJTKGUFSWMDOENXPAHIBLCR
PWKCBAMRHQINUSFTVYGLJEODX
JVCKWEBHOGQRLFTPDIXUANMSY
QBERMWFPAUJXNIHGYKSCVOTLD
STHPLIXQRYODVGUMEAWNCJFKB
UXDNFCTMVKSEAYWOJRLBPHGQI
OGIAYNLDSJCKPBMHQFVTWRUXE
(0.14 secs, 60,797,176 bytes)
-}    
    
xh2 = 
   ["..U.R.Q.E..T.V..B.N.SIC..",
    "..LXO...BC.IWQ.VG...KTJ..",
	  "GEB.Q..JM.H...P.TD..O.NWV",
	  "CA.....XT.NF.MO.KH.....YL",
	  "IJT..UK.P.GS.LB.O.YE..HFQ",
	  "....VS...X.L.P.J...GH....",
	  "D...P.IBU.KO.NA.SCF.Q...J",
	  "..FO..H..LU.Y.CR..D..VS..",
	  "EY.UJ.P.CN.....XW.O.LG.AF",
	  ".CM..F.TG..B.J..AQ.V..PU.",
	  "..SDK.JO.........VM.RQX..",
	  "TF.MX.SN.E.....B.GI.YP.VU",
	  ".H.....L.........N.....O.",
	  "RL.B.X.Y.V.....D.OK.EC.HG",
	  "..VEU.CG.........ST.MLI..",
	  ".DN..J.WL..C.A..UE.O..YM.",
	  "LS.FA.E.NO.....CM.Q.UK.PH",
	  "..OG..D..BV.H.LF..R..AQ..",
	  "V...T.GUF.WM.OE.XPA.I...R",
	  "....BA...Q.N.S.T...LJ....",
	  "JVC..EB.O.QR.FT.D.XU..MSY",
	  "QB.....PA.JX.IH.YK.....LD",
	  "STH.L..QR.O...U.EA..C.FKB",
	  "..DNF...VK.EAY.OJ...PHG..",
	  "..IAY.L.S..K.B..Q.V.WRU.."]
{-
"?: " doit xh2 solver6
HKUYROQFEDATJVXWBLNMSICGP
FMLXOHNSBCYIWQDVGUPRKTJEA
GEBSQLYJMAHUKRPITDCFOXNWV
CAPVDGWXTINFEMOSKHJQBURYL
IJTWNUKVPRGSCLBAOXYEDMHFQ
AIWTVSOEYXFLMPQJNBUGHDKRC
DRXHPVIBUMKOGNALSCFYQWETJ
BNFOGQHAJLUWYECRPTDKXVSIM
EYQUJDPKCNRVTHSXWMOILGBAF
KCMLSFRTGWDBXJIEAQHVNYPUO
YPSDKTJOIHEAFCGULVMWRQXBN
TFAMXRSNQELHODKBCGIJYPWVU
WHGQCMALKPBJIUVYRNEXFSDOT
RLJBIXUYWVMQSTNDFOKPECAHG
NOVEUBCGDFXPRWYQHSTAMLIJK
XDNIHJVWLTPCQARKUEBOGFYMS
LSRFAYEINOTGBXJCMWQDUKVPH
MUOGEPDCXBVYHKLFIJRSTAQNW
VQYJTKGUFSWMDOENXPAHIBLCR
PWKCBAMRHQINUSFTVYGLJEODX
JVCKWEBHOGQRLFTPDIXUANMSY
QBERMWFPAUJXNIHGYKSCVOTLD
STHPLIXQRYODVGUMEAWNCJFKB
UXDNFCTMVKSEAYWOJRLBPHGQI
OGIAYNLDSJCKPBMHQFVTWRUXE
(43.59 secs, 21,561,426,272 bytes)
-}      
