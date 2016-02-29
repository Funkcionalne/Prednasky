module Cartesian where

-- author (asi) Marian Vysluzil
--rozsirena verzia iba za pomoci foldr/foldl a (:).....
--cart [ [1,2], [3,4], [5] ]
cart  :: [[t]] -> [[t]]
cart xss = foldr (\x y -> cartTemp2 x y) [[]] xss
 
 
-- prvky jednej mnoziny kombinujeme s mnohymi mnozinami
--cartTemp2 [1, 2, 3] [[4, 5],[6,7],[8,9]] == [[1,4,5],[1,6,7],[1,8,9],[2,4,5],[2,6,7],[2,8,9],[3,4,5],[3,6,7],[3,8,9]]
cartTemp2 :: [t] -> [[t]] -> [[t]]
cartTemp2 [] _ = []
cartTemp2 xs yss = foldr (\x y -> (foldr (:) (cartTemp x yss) y)) [] xs

cartTemp2' :: [t] -> [[t]] -> [[t]]
cartTemp2' xs yss = concat [ cartTemp x yss | x<-xs]
 
-- pridame jeden prvok do kazdej mnoziny 
--cartTemp 1 [[4,5],[6,7]] == [[1,4,5],[1,6,7]]
cartTemp :: t -> [[t]] -> [[t]]
-- slusne napisane:
--cartTemp element xss = map (element:) xss
{- Marianov original, untouched !
cartTemp _ [] = []
cartTemp element (xs:xss) = (element:xs):(cartTemp element xss)
-}
-- trochu prepisane, bez rekurzie
-- cartTemp element xss = foldr (\xs rekurzia -> (element:xs):rekurzia) [] xss
-- este trochu prepisane

cartTemp element = foldr pom [] 
                  where 
                    pom xs rek = (element:xs):rek
