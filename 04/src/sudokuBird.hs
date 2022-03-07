boxsize :: Int
boxsize = 5
cellvals 
  | boxsize == 3 = "123456789"
  | boxsize == 4 = "123456789abcdefh"
  | boxsize == 5 = "ABCDEFGHIJKLMNOPQRSTUVWXY"
blank = (=='.')

-- Matrix is list of rows
type Matrix a = [[a]]
type Board = Matrix Char

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

-- in fact it is transpose, we do list of columns
cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

--groupBy :: Integer -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)  

group :: [a] -> [[a]]
group = groupBy boxsize

ungroup :: [[a]] -> [a]
ungroup = concat

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

correct :: Board -> Bool
correct b = all nodups (rows b) &&
            all nodups (cols b) &&
			all nodups (boxs b) -- and eventually even diagonals
			
type Choices = [Char]

choices :: Board -> Matrix Choices
choices = map (map choose)
choose e = if blank e then cellvals else [e]

--mcp :: Matrix [a] -> [Matrix a]
--mcp = cp . map cp

--cp :: [[a]] -> [[a]]
--cp [] = [[]]
--cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

single :: [a] -> Bool
single [x] = True
single _ = False
--single x = length x == 1

fixed :: [Choices] -> Choices
fixed = concat . filter single

--delete :: Choices -> Choices
delete :: Eq a => [a] -> [a] -> [a]
delete fs cs = filter (notIn fs) cs
notIn fs x = notElem x fs

-- delOne e [] = []
-- delOne e (x:xs) = if e == x then delOne e xs else x:(delOne e xs)

reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css
remove fs cs = if single cs then cs else delete fs cs

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneBy :: (Matrix Choices -> Matrix Choices) -> (Matrix Choices -> Matrix Choices)
pruneBy f = f . map reduce . f

blocked :: Matrix Choices -> Bool
blocked cm = void cm || not (safe cm)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe cm = all (nodups . fixed) (rows cm) &&
          all (nodups . fixed) (cols cm) &&
		  all (nodups . fixed) (boxs cm)
		  
expand cm = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
   where 
     (rows1, row:rows2) = break (any best) cm
     (row1, cs: row2)   = break best row
     best cs            = length cs == n				   
     n                  = minchoice cm
				   
minchoice = minimum . filter (/=1) . map length . concat	

search :: Matrix Choices -> [Matrix Choices]
search cm			   
  | blocked cm   = []
  | complete cm  = [cm]
  | otherwise    = (concat . map (search . prune) . expand) cm

--sudoku :: Board -> [Board]
sudoku = map (map concat) . search . prune . choices
  
complete = all(all single)

safe1 m = all ok (rows m) && all ok (cols m) && all ok (boxs m)

ok row = nodups [d | [d] <- row]  
  
sudoku1 = search1 . choices

--search1 :: Matrix Choices -> [Matrix Choices]
search1 m 
  | not (safe1 m1) = []
  | complete m1    = [map (map head) m1]
  | otherwise      = concat (map search1 (expand m1))
  where m1 = prune m

--------------------- upravene sudoku  

sudoku2 = mysearch1 . choices

myprune :: Matrix Choices -> Matrix Choices
myprune = mypruneBy boxs . mypruneBy cols . mypruneBy rows

mypruneBy :: (Matrix Choices -> Matrix Choices) -> (Matrix Choices -> Matrix Choices)
mypruneBy f = f . map xreduce . map reduce . f

--search1 :: Matrix Choices -> [Matrix Choices]
mysearch1 m 
  | not (safe1 m1) = []
  | complete m1    = [map (map head) m1]
  | otherwise      = concat (map mysearch1 (expand m1))
  where m1 = fup m

xreduce m = map unique m
  where
    unique e 
      | single e   = e
      | null r     = e
      | otherwise  = r
        where r    = check e m

check [] m  = []
check (x:xs) m 
  | single (filter (elem x) m) = [x]
  | otherwise                  = check xs m

fup m | pm /= m   = fup pm
      | otherwise = m
      where pm = myprune m  

m2 = [".52769..8",
      "76.8.3.9.",
	  ".83.4....",
	  "..4......",
	  ".2961487.",
	  "......5..",
	  "....7.231",
	  ".4.5.2.87",
	  "2..19864."]

m3 = ["86..9...5", --108
      ".5.8.....",
      "73.1..8..",
      "..54...7.",
      ".........",
      ".8...36..",
      "..2..5.19",
      ".....9.2.",
      "3...1..56"]
	  
m4 = ["263.1.5..", --101
      "...54.2..",
      ".....2...",
      "....91..2",
      ".46...81.",
      "7..48....",
      "...9.....",
      "..1.34...",
      "..8.2.469"]	  

m5 = ["17.......",
      ".85.293..",
      "3..8..257",
      "634..8592",
      ".........",
      "7594..138",
      "921..5..3",
      "..394.82.",
      ".......65"]
	  
m6 = ["6....4...",   --tazke fup nevyriesi
      "..8..7...",
      "71..8.9..",
      ".3...57.1",
      ".8.....2.",
      "5.79...6.",
      "..1.6..72",
      "...4..5..",
      "...5....9"]
	  
m7 = ["...7.6.8.",
      "..6.59.13",
      ".......9.",
      "95....12.",
      "....7....",
      ".61....39",
      ".3.......",
      "24.59.3..",
	  ".8.1.2..."]	  

	  
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
xh2 = ["..U.R.Q.E..T.V..B.N.SIC..",
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
-- >>Solution1
-- ONVMSWQAGEJFCTBUIKDLRHXPY
-- HKELCRDSJNMGYOIVBXPWQUAFT
-- BAFUYTXVMLPREHWQCNSGJKDIO
-- QGIRWPUOHYDVLXKTAEFJMSCBN
-- XPDJTIKFCBSQNAUYHORMLEWVG
-- YBONVXARSQWPMCGFETHKUIJDL
-- ARJPDEHIKCTXFULMNVOBYQGSW
-- EQSXMVFTWGIODBJRLCYUNAKHP
-- CTKWLUJYPDNERSHIXAGQOVFMB
-- GUHIFNLBOMVYQKASPJWDXCTRE
-- LHQVBKOPESCNUMFWTIJAGYRXD
-- TYNOXJWGQIAHBRDKULEPFMSCV
-- FEUSJDCMXRKLPQTBGHVYWOINA
-- DIRCPATULVYWGJOXFSMNKBHEQ
-- MWAKGBNHYFXIVESODRQCTPLUJ
-- JCTQUSRWDHBMIFVLOYXEPGNAK
-- IDBGHFYCNUOKJWRPSMAVELQTX
-- VLMYKOPQTAECHNXGRWUIDFBJS
-- RSPANMELVXGDTYQJKBCFHWUOI
-- WOXFEGIKBJUASLPNQDTHCRVYM
-- NMWTRHBJUKLSOPYCVQIXADEGF
-- PFLBQYSDATHJKICEMGNRVXOWU
-- UVGHICMXFOQTWDEAJPKSBNYLR
-- KJCEALGNIWRUXVMDYFBOSTPQH
-- SXYDOQVERPFBAGNHWULTIJMKC
-- >>Solution2
-- HWRTXGECFVYQMIUOAPKDJSNLB
-- QSOVYIPBDKAFGCEJLNWRXTUHM
-- LUAJCHNRYSDXOWKMTQBIPEGFV
-- FGEDBLJMAONRPTHXVYUSQIKCW
-- MKINPQUTWXJSLVBEGHFCRYODA
-- AXLEGTFIJWRPSQYNOCHKMBVUD
-- CMVINOXGUALJDKFRWBSYHPQET
-- RPQKHMVDNYBITXOAJLEUWCSGF
-- YBWOSCQHLPEUANGTFDMVKJIRX
-- JTDFUKSEBRWCVHMPQIGXONLAY
-- GEHYKPBJSNTVXMCQDRIFUWAOL
-- ILJPAWMOGTSEFRQUNXCBYVDKH
-- DOCMTXLVRFIBKUAYHWJGNQESP
-- XNSWRUCQKEPDHYLVMTOAIGFBJ
-- BQFUVDAYIHGWNOJLKSPETXRMC
-- OHBLIVRNMJQGEPSFYKDWCATXU
-- KDPCJBTFOGMLIAWHUEXNSRYVQ
-- NFYSMEDXQIUKJBVCRATPGLHWO
-- VATGQYWSCUOHRFXBIJLMEDPNK
-- WRUXEAHKPLCTYDNGSOVQBFMJI
-- TYXRDJKAEMVOBSPICUNLFHWQG
-- EIGAWNOUHDFYCJRKBVQTLMXPS
-- UJNQOFGLTBXAWEDSPMYHVKCIR
-- SCMHLRIPVQKNUGTWXFAJDOBYE
-- PVKBFSYWXCHMQLIDEGROAUJTN
-- >>Solution3
-- HNAOBKLFDMQSTPJVGRXIUYCWE
-- SDFCMRTPHWIGEYVNUJAQOXLBK
-- VYJXEIQCGUDLFRNOWMBKTPAHS
-- UILPRSYVBNOWXAKEHFTCMJGQD
-- KTQWGJXEAOCHBMUDYPSLVNFIR
-- TVWYXCDSJBNMPOHQALRUFEKGI
-- BCKSNWROQILFGJAPXEVDHTMYU
-- MRULQEGNPVKTIXSWFHYODBJAC
-- DFEGIUHXLAVYCBWSTKMJQRPNO
-- AOPJHYKMTFRDUEQIBCGNXLSVW
-- YKVBWXUJCRSEALGTPOFMIHQDN
-- EHIULPMQNYXRJFCGVDKWASTOB
-- OPGMDASLIKHUWTBXJNQYECVRF
-- CXNQSTVBFHYKOIDUEALRJMWPG
-- FJTRAGOWEDPNQVMHISCBLUXKY
-- QGOAPNCKUXMIYSFBRVETWDHLJ
-- NSMITFJRVPGBDCLKQXWHYOUEA
-- RECFJBIHSQWPKUTLOYDAGVNMX
-- XWDVUOAGYLJQRHECMINFPKBST
-- LBYHKMEDWTAXVNOJSUPGCIRFQ
-- GQSEFDNAMCUOHKRYLTJPBWIXV
-- PMBNCLFYKJTVSQIADWOXRGEUH
-- JUHDYQBTRSEALWXMNGIVKFOCP
-- ILRTOVWUXGBCNDPFKQHESAYJM
-- WAXKVHPIOEFJMGYRCBUSNQDTL
-- >> Solution 4	  
-- H K U Y R O Q F E D A T J V X W B L N M S I C G P
-- F M L X O H N S B C Y I W Q D V G U P R K T J E A
-- G E B S Q L Y J M A H U K R P I T D C F O X N W V
-- C A P V D G W X T I N F E M O S K H J Q B U R Y L
-- I J T W N U K V P R G S C L B A O X Y E D M H F Q
-- A I W T V S O E Y X F L M P Q J N B U G H D K R C
-- D R X H P V I B U M K O G N A L S C F Y Q W E T J
-- B N F O G Q H A J L U W Y E C R P T D K X V S I M
-- E Y Q U J D P K C N R V T H S X W M O I L G B A F
-- K C M L S F R T G W D B X J I E A Q H V N Y P U O
-- Y P S D K T J O I H E A F C G U L V M W R Q X B N
-- T F A M X R S N Q E L H O D K B C G I J Y P W V U
-- W H G Q C M A L K P B J I U V Y R N E X F S D O T
-- R L J B I X U Y W V M Q S T N D F O K P E C A H G
-- N O V E U B C G D F X P R W Y Q H S T A M L I J K
-- X D N I H J V W L T P C Q A R K U E B O G F Y M S
-- L S R F A Y E I N O T G B X J C M W Q D U K V P H
-- M U O G E P D C X B V Y H K L F I J R S T A Q N W
-- V Q Y J T K G U F S W M D O E N X P A H I B L C R
-- P W K C B A M R H Q I N U S F T V Y G L J E O D X
-- J V C K W E B H O G Q R L F T P D I X U A N M S Y
-- Q B E R M W F P A U J X N I H G Y K S C V O T L D
-- S T H P L I X Q R Y O D V G U M E A W N C J F K B
-- U X D N F C T M V K S E A Y W O J R L B P H G Q I
-- O G I A Y N L D S J C K P B M H Q F V T W R U X E
-- >> Solution 1
-- O N V M S W Q A G E J F C T B U I K D L R H X P Y
-- H K E L C R D S J N M G Y O I V B X P W Q U A F T
-- B A F U Y T X V M L P R E H W Q C N S G J K D I O
-- Q G I R W P U O H Y D V L X K T A E F J M S C B N
-- X P D J T I K F C B S Q N A U Y H O R M L E W V G
-- Y B O N V X A R S Q W P M C G F E T H K U I J D L
-- A R J P D E H I K C T X F U L M N V O B Y Q G S W
-- E Q S X M V F T W G I O D B J R L C Y U N A K H P
-- C T K W L U J Y P D N E R S H I X A G Q O V F M B
-- G U H I F N L B O M V Y Q K A S P J W D X C T R E
-- L H Q V B K O P E S C N U M F W T I J A G Y R X D
-- T Y N O X J W G Q I A H B R D K U L E P F M S C V
-- F E U S J D C M X R K L P Q T B G H V Y W O I N A
-- D I R C P A T U L V Y W G J O X F S M N K B H E Q
-- M W A K G B N H Y F X I V E S O D R Q C T P L U J
-- J C T Q U S R W D H B M I F V L O Y X E P G N A K
-- I D B G H F Y C N U O K J W R P S M A V E L Q T X
-- V L M Y K O P Q T A E C H N X G R W U I D F B J S
-- R S P A N M E L V X G D T Y Q J K B C F H W U O I
-- W O X F E G I K B J U A S L P N Q D T H C R V Y M
-- N M W T R H B J U K L S O P Y C V Q I X A D E G F
-- P F L B Q Y S D A T H J K I C E M G N R V X O W U
-- U V G H I C M X F O Q T W D E A J P K S B N Y L R
-- K J C E A L G N I W R U X V M D Y F B O S T P Q H
-- S X Y D O Q V E R P F B A G N H W U L T I J M K C
-- >> Solution 2
-- H W R T X G E C F V Y Q M I U O A P K D J S N L B
-- Q S O V Y I P B D K A F G C E J L N W R X T U H M
-- L U A J C H N R Y S D X O W K M T Q B I P E G F V
-- F G E D B L J M A O N R P T H X V Y U S Q I K C W
-- M K I N P Q U T W X J S L V B E G H F C R Y O D A
-- A X L E G T F I J W R P S Q Y N O C H K M B V U D
-- C M V I N O X G U A L J D K F R W B S Y H P Q E T
-- R P Q K H M V D N Y B I T X O A J L E U W C S G F
-- Y B W O S C Q H L P E U A N G T F D M V K J I R X
-- J T D F U K S E B R W C V H M P Q I G X O N L A Y
-- G E H Y K P B J S N T V X M C Q D R I F U W A O L
-- I L J P A W M O G T S E F R Q U N X C B Y V D K H
-- D O C M T X L V R F I B K U A Y H W J G N Q E S P
-- X N S W R U C Q K E P D H Y L V M T O A I G F B J
-- B Q F U V D A Y I H G W N O J L K S P E T X R M C
-- O H B L I V R N M J Q G E P S F Y K D W C A T X U
-- K D P C J B T F O G M L I A W H U E X N S R Y V Q
-- N F Y S M E D X Q I U K J B V C R A T P G L H W O
-- V A T G Q Y W S C U O H R F X B I J L M E D P N K
-- W R U X E A H K P L C T Y D N G S O V Q B F M J I
-- T Y X R D J K A E M V O B S P I C U N L F H W Q G
-- E I G A W N O U H D F Y C J R K B V Q T L M X P S
-- U J N Q O F G L T B X A W E D S P M Y H V K C I R
-- S C M H L R I P V Q K N U G T W X F A J D O B Y E
-- P V K B F S Y W X C H M Q L I D E G R O A U J T N
-- >> Solution 3
-- H N A O B K L F D M Q S T P J V G R X I U Y C W E
-- S D F C M R T P H W I G E Y V N U J A Q O X L B K
-- V Y J X E I Q C G U D L F R N O W M B K T P A H S
-- U I L P R S Y V B N O W X A K E H F T C M J G Q D
-- K T Q W G J X E A O C H B M U D Y P S L V N F I R
-- T V W Y X C D S J B N M P O H Q A L R U F E K G I
-- B C K S N W R O Q I L F G J A P X E V D H T M Y U
-- M R U L Q E G N P V K T I X S W F H Y O D B J A C
-- D F E G I U H X L A V Y C B W S T K M J Q R P N O
-- A O P J H Y K M T F R D U E Q I B C G N X L S V W
-- Y K V B W X U J C R S E A L G T P O F M I H Q D N
-- E H I U L P M Q N Y X R J F C G V D K W A S T O B
-- O P G M D A S L I K H U W T B X J N Q Y E C V R F
-- C X N Q S T V B F H Y K O I D U E A L R J M W P G
-- F J T R A G O W E D P N Q V M H I S C B L U X K Y
-- Q G O A P N C K U X M I Y S F B R V E T W D H L J
-- N S M I T F J R V P G B D C L K Q X W H Y O U E A
-- R E C F J B I H S Q W P K U T L O Y D A G V N M X
-- X W D V U O A G Y L J Q R H E C M I N F P K B S T
-- L B Y H K M E D W T A X V N O J S U P G C I R F Q
-- G Q S E F D N A M C U O H K R Y L T J P B W I X V
-- P M B N C L F Y K J T V S Q I A D W O X R G E U H
-- J U H D Y Q B T R S E A L W X M N G I V K F O C P
-- I L R T O V W U X G B C N D P F K Q H E S A Y J M
-- W A X K V H P I O E F J M G Y R C B U S N Q D T L
-- >> Solution 4	  
  