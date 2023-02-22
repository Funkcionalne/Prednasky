module Goo where
import Data.Bits as DB
import Data.List
import Test.QuickCheck

-- slušák, čo pobral niečo z prednášky
goo :: Int -> Int -> Int
goo 0 b = 0 
goo a b | a `mod` 2 == 0 =     goo (a `div` 2) (2*b) 
        | otherwise      = b + goo (a `div` 2) (2*b)

gooo :: Int -> Int -> Int
gooo 0 b = 0 
gooo a b | a `mod` 2 == 0 =     aux
         | otherwise      = b + aux
          where aux = goo (a `div` 2) (2*b)

gooo' :: Int -> Int -> Int
gooo' 0 b = 0 
gooo' a b  = let aux = goo (a `div` 2) (2*b) in
             aux + if a `mod` 2 == 0 then 0 else b

-- ľavičiarsky akumulátorčík
goo' :: Int -> Int -> Int
goo' a b = foldl (\acc -> \(a,b) -> acc + if a `mod` 2 > 0 then b else 0) 0 $
             takeWhile ((>0).fst) $ 
               iterate (\(a,b) -> (a `div` 2, 2*b)) (a,b)

{-
take 10 $ iterate (\(a,b) -> (a `div` 2, 2*b)) (17,21)
takeWhile ((>0).fst) $ iterate (\(a,b) -> (a `div` 2, 2*b)) (17,21)
filter ((>0).(`mod`2).fst) $ takeWhile ((>0).fst) $ iterate (\(a,b) -> (a `div` 2, 2*b)) (17,21)
map (snd) $filter ((>0).(`mod`2).fst) $ takeWhile ((>0).fst) $ iterate (\(a,b) -> (a `div` 2, 2*b)) (17,21)
sum $ map (snd) $filter ((>0).(`mod`2).fst) $ takeWhile ((>0).fst) $ iterate (\(a,b) -> (a `div` 2, 2*b)) (17,21)
-}
-- bojazlivejší čo pozná map filter z Pythonu
goo'' :: Int -> Int -> Int
goo'' a b = sum $
              map snd $
                filter ((>0).(`mod` 2).fst) $
                  takeWhile ((>0).fst) $ iterate (\(a,b) -> (a `div` 2, 2*b)) (a,b)

-- chce provokovať prednášajúceho
goo''' :: Int -> Int -> Int
goo''' a b = sum $
               map snd $
                 filter ((\a -> a DB..&. 1>0).fst) $
                   takeWhile ((>0).fst) $ 
                     iterate (\(a,b) -> (shiftR a 1, shiftL b 1)) (a,b)            
                     
hypotheza1   = quickCheck(\a -> \b -> 
                    a >= 0 ==> goo a b == goo' a b)
hypotheza2   = quickCheck(\a -> \b -> 
                    a >= 0 ==> goo a b == goo' a b)
hypotheza3   = quickCheck(\a -> \b -> 
                    a >= 0 ==> length ( nub [goo a b, goo' a b, goo'' a b, goo''' a b]) == 1)

hypotheza4   = quickCheck(\a -> \b -> 
                    a >= 0 ==> goo a b == gooo' a b)
                     