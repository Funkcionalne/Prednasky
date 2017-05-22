module Piskvorky where

-- hra sa spusta takto:

-- jeden hrac proti kompu
-- hra1 True 'x' prazdna

-- dvaja hraci bez kompu
-- hra2 'x' prazdna


-- kvoli konverzii ord::Char->Int
--import Char
import Data.Char

-- zakladne typy
type Matica 	= [String] 
type Tah 		= String

-- predpokladame stvorcovu hraciu plochu so suradnicami: suradnice x suradnice
-- rozmery hracej plochy su 17x17
velkost				:: Int
velkost				= 17
velkostMinusJedna 	= velkost-1

suradnice	:: String
suradnice 	= map chr [ord 'A' .. ord 'A'+velkostMinusJedna] -- inak povedane = "ABCDEFGHIJKLMNOPQRS"

-------------------------------

-- vykreslovanie hracej plochy
toString :: Matica -> String
toString p =  
  	"#|" ++ vlozMedzery suradnice ++ "|\n" ++
    "-+----------------------------------|\n" ++
    (foldr (++) [] [suradnice!!i:("|" ++ vlozMedzery (p!!i) ++ "|\n") | i<-[0..velkostMinusJedna]]) ++
    "-+----------------------------------+" 

-- vloz kazdy druhy znak medzeru    
vlozMedzery			:: String -> String
vlozMedzery []		= []                       
vlozMedzery (x:xs) 	= x:' ':vlozMedzery xs

-- protihrac hraca
proti	:: Char -> Char
proti	'x'	= 'o'
proti	'o' = 'x'
       
-- prazdna plocha, inicialny stav hry   
prazdna 	:: Matica
prazdna 	= take velkost (repeat (take velkost (repeat '.')))

-- hracia Plocha obsahuje piskvorku daneho hraca, o, resp x					
najdiPisky 	:: Char->Matica->Bool
najdiPisky hrac m = 
				  (or (map (najdiPiskyRiadok hrac) m )) 
				  ||
				  (or (map (najdiPiskyRiadok hrac) (transpose m) )) 
				  ||
				  (najdiPiskyDiag hrac m )
				  ||
				  (najdiPiskyDiag hrac (map reverse m))

najdiPiskyRiadok :: Char->String->Bool
najdiPiskyRiadok _ []			= False
najdiPiskyRiadok _ [_]			= False
najdiPiskyRiadok _ [_,_]		= False
najdiPiskyRiadok _ [_,_,_]		= False
najdiPiskyRiadok _ [_,_,_,_]	= False
najdiPiskyRiadok x (h1:rest@(h2:h3:h4:h5:xs)) | x==h1 && x==h2 && x==h3 && x== h4 && x==h5 = True
										  	  | otherwise = najdiPiskyRiadok x rest

najdiPiskyDiag :: Char->Matica->Bool
najdiPiskyDiag hrac m = or [
						m!!i!!j == hrac &&
						m!!(i+1)!!(j+1) == hrac &&
						m!!(i+2)!!(j+2) == hrac &&
						m!!(i+3)!!(j+3) == hrac &&
						m!!(i+4)!!(j+4) == hrac | i<-[0..velkost-5], j<-[0..velkost-5] ]

-- z prednasky						
transpose	:: Matica -> Matica		
transpose	[]			= []
transpose  	([]:xss) 	= transpose xss
transpose	((x:xs):xss) = (x:(map head xss)):(transpose (xs:(map tail xss)))

-- zisti, ci je este nejake policko, kam je mozne tahat, dovtedy hra nekocni
jeRemiza	:: Matica -> Bool
jeRemiza 	= foldr (\r -> \y -> (and (map (/='.') r)) && y) True 

-- zapise tah do hracej plochy
poznacTah :: Char -> String -> Matica -> Matica
poznacTah hrac tah m = 
			[ [ if (i==riadok && j == stlpec) then hrac else m!!i!!j | j<-[0..(length m)-1] ] | i<-[0..(length m)-1] ]
			where (riadok, stlpec) = dekodujTah tah

-- hraju dvaja hraci, proti sebe, zacina 'hrac'		
hra2		:: Char -> Matica -> IO()
hra2 hrac  m =
		if jeRemiza m then
			putStrLn "R.E.M.I.Z.A"
		else 
          do		
 		    putStrLn ("\ESC[2J"++(toString m))
		    putStrLn (hrac:", zadaj tvoj tah (riadok,stlpec):")
		    tah <- getLine		-- nacitaj jeho tah
		    if (jeKorektnyTah tah m) then	
		      let m' = (poznacTah hrac tah m) in
		        if najdiPisky hrac m' then		-- ak dal piskvorku
		  	      putStrLn (hrac:" vyhral")		-- tak vyhral
		        else 							-- inak ide jeho super
		          (hra2 (proti hrac) (poznacTah hrac tah m'))
		    else	-- zadal nekorektny tah
		      do
		        putStrLn "! nekorektny tah !"
		        (hra2 hrac m)	-- hrac dostane dalsiu sancu
		    
-- tah jeKorektny, ak su suradnice v intervale suradnic a policko nie je obsadene					
jeKorektnyTah			::	String -> Matica -> Bool
jeKorektnyTah tah m		= 	(length tah >= 2) &&
						   	riadok >= 0 && riadok < length suradnice &&
							stlpec >= 0 && stlpec < length suradnice &&
							polickoJeVolne riadok stlpec m
						where (riadok,stlpec) = dekodujTah tah

polickoJeVolne					:: Int -> Int -> Matica -> Bool
polickoJeVolne	riadok stlpec m = m!!riadok!!stlpec == '.'
		    					
-- tah ma tvar dvoch znakov, suradnica riadku a stlpca na ploche
dekodujTah			:: String -> (Int, Int)
dekodujTah	tah		= (ord (toUpper (tah!!0)) - ord 'A', ord (toUpper (tah!!1)) - ord 'A')

kodujTah			:: (Int, Int) -> String
kodujTah (riadok, stlpec) =	[chr (riadok + ord 'A'),chr (stlpec + ord 'A')]

---------------------------------

-- hra jeden hrac, proti funkcii piskvorky
hra1	:: Bool->Char -> Matica -> IO()
hra1	hracNaTahu hrac  m = 
		if jeRemiza m then
			putStrLn "R.E.M.I.Z.A"
		else if hracNaTahu then	-- ak je clovek-hrac na tahu
	  	  do
			putStrLn ("\ESC[2J"++(toString m))
			putStrLn (hrac:", zadaj tvoj tah (riadok,stlpec):")
			tah <- getLine	-- nacitaj jeho tah
			if jeKorektnyTah tah m then
			  let m' = (poznacTah hrac tah m) in
		  		if najdiPisky hrac m' then
		  			putStrLn (hrac:" vyhral")
		  		else 			-- na tahu je hrac-pocitac
				(hra1 (not hracNaTahu) (proti hrac) m')
			else
		  	   do
		        putStrLn "! nekorektny tah !"
		    	(hra1 hracNaTahu hrac m)	-- hrac dostane dalsiu sancu
		else	-- na tahu je hrac-pocitac
	  	  do 
	  		tah <- return (piskvorky hrac m)	-- zavolaj fuknciu, ktoru mate naprogramovat dole !!!
			putStrLn (hrac:", isiel na " ++ tah) 
			if jeKorektnyTah tah m then		-- pocitac musi tiez hrat korektne
			  let m' = (poznacTah hrac tah m) in
		  	  	if najdiPisky hrac  m' then
		  			putStrLn (hrac:" vyhral")
		  		else 	
		    		(hra1 (not hracNaTahu) (proti hrac) m')
			else	-- pocitac nema druhu sancu, ma hadat poriadne !
		  		putStrLn ("! nekorektny tah !\n" ++ (hrac:" prehral"))

-- nasledujuci riadok nechajte a za nim uvedte svoju verziu funkcie piskvorky
-- tic-tac-toe

piskvorky			:: Char -> Matica -> Tah
piskvorky hrac m	=  kodujTah (head [ (i,j) | i<-[0..velkostMinusJedna], j<-[0..velkostMinusJedna],  m!!i!!j == '.' ])

cls :: IO ()
cls = putStr "\ESC[2J" 

-- naivna strategia, najde prve volne policko a tam ide

