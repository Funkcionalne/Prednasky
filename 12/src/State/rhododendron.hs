data Tree a = Node a [Tree a]  deriving (Show, Eq)

e1 :: Tree String
e1 = Node "Jano" [	(Node "Fero"  [(Node "a" []),(Node "b" []),(Node "a" []),(Node "b" [])]),
			(Node "Jano"  [(Node "a" [])]),
			(Node "Karel"  [(Node "c" []),(Node "a" []),(Node "c" [])]),
			(Node "Fero"  [(Node "d" []),(Node "b" []),(Node "a" []),(Node "c" [])]),
			(Node "Karel"  [(Node "d" []),(Node "a" []),(Node "d" [])])
		]

-- spocita velkost stromu - len preto, aby ste si vyskusali rekurziu na Rhododendrone, alias Tree a.    
size :: Tree a -> Int
    
-- vytvori identicky strom, pricom hodnoty vrcholov su postupne iduce prirodzene cisla 0,1,...
-- na sposobe prechadzania stromu nezalezi    
reindex :: Tree a -> Tree Int

reindex e1 =
Node 0 [	(Node 1  [(Node 2 []),(Node 3 []),(Node 4 []),(Node 5 [])]),
			(Node 6  [(Node 7 [])]),
			(Node 8  [(Node 9 []),(Node 10 []),(Node 11 [])]),
			(Node 12  [(Node 13 []),(Node 14 []),(Node 15 []),(Node 16 [])]),
			(Node 17  [(Node 18 []),(Node 19 []),(Node 20 [])])
		]

-- vytvori identicky strom, pricom hodnoty vrcholov su prirodzene cisla 0,1,...
-- s tym dolezitym rozdielom, ze rovnake hodnoty typu ::a maju rovnake indexy, cisla
rename  :: Tree a -> Tree Int

rename e1 =
Node 0 [	(Node 1  [(Node 2 []),(Node 3 []),(Node 2 []),(Node 3 [])]),
			(Node 0  [(Node 2 [])]),
			(Node 4  [(Node 5 []),(Node 2 []),(Node 5 [])]),
			(Node 1  [(Node 6 []),(Node 3 []),(Node 2 []),(Node 5 [])]),
			(Node 4  [(Node 6 []),(Node 2 []),(Node 6 [])])
		]  -- cislovanie sa moze lisit od vasej implementacie
