module Cartesian where

-- iniciálne riešenie
cp_1 []         = [[]]
cp_1 (xs:xss)   = [(x:ys) | x <- xs, ys <- cp_1 xss]

-- rozbité na vnútorný a vonkajší list-comprehension
cp_2 []         = [[]]
cp_2 (xs:xss)   = concat [ [(x:ys) | ys <- cp_2 xss ] | x <- xs]

-- vnútorný list=comprehension prepíšeme cez map
cp_3 []         = [[]]
cp_3 (xs:xss)   = concat [ map (x:) (cp_3 xss) | x <- xs]

-- zavedieme foldr
cp_4 xss   = foldr pom [[]] xss 
             where
                pom xs rek = concat [ map (x:) rek | x <- xs]

-- odstránime concat
cp_5 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (\x -> \rek2 -> (map (x:) rek) ++ rek2) [] xs

-- slušnejšie prepísané
cp_6 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (pom2 rek) [] xs
                pom2 rek x rek2 = (map (x:) rek) ++ rek2

-- odstránime map
cp_7 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (pom2 rek) [] xs
                pom2 rek x rek2 = (foldr (pom3 x) [] rek) ++ rek2
                pom3 x y ys = (x:y):ys
                
-- odstránime append
cp_8 xss   = foldr pom [[]] xss 
             where
                pom xs rek = foldr (pom2 rek) [] xs
                pom2 rek x rek2 = foldr (:) rek2 (foldr (pom3 x) [] rek)
                pom3 x y ys = (x:y):ys
                
-- jediný problém, že to ide aj s tromi foldami
-- Strachey's functional pearl, forty years on
https://spivey.oriel.ox.ac.uk/mike/firstpearl.pdf