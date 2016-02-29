module Cartesian where

-- inici�lne rie�enie
cp_1 []         = [[]]
cp_1 (xs:xss)   = [(x:ys) | x <- xs, ys <- cp_1 xss]

-- rozbit� na vn�torn� a vonkaj�� list-comprehension
cp_2 []         = [[]]
cp_2 (xs:xss)   = concat [ [(x:ys) | ys <- cp_2 xss ] | x <- xs]

-- vn�torn� list=comprehension prep�eme cez map
cp_3 []         = [[]]
cp_3 (xs:xss)   = concat [ map (x:) (cp_3 xss) | x <- xs]

-- zavedieme foldr
cp_4 xss   = foldr pom [[]] xss 
             where
                pom xs rek = concat [ map (x:) rek | x <- xs]

-- odstr�nime concat
cp_5 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (\x -> \rek2 -> (map (x:) rek) ++ rek2) [] xs

-- slu�nej�ie prep�san�
cp_6 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (pom2 rek) [] xs
                pom2 rek x rek2 = (map (x:) rek) ++ rek2

-- odstr�nime map
cp_7 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (pom2 rek) [] xs
                pom2 rek x rek2 = (foldr (pom3 x) [] rek) ++ rek2
                pom3 x y ys = (x:y):ys
                
-- odstr�nime append
cp_8 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (pom2 rek) [] xs
                pom2 rek x rek2 = foldr (:) rek2 (foldr (pom3 x) [] rek)
                pom3 x y ys = (x:y):ys
                
-- jedin� probl�m, �e to ide aj s tromi foldami
-- Strachey's functional pearl, forty years on
https://spivey.oriel.ox.ac.uk/mike/firstpearl.pdf