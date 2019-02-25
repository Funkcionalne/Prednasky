module QuickSort where

import Test.QuickCheck
import Data.List (sort)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter  (< x) xs) 
               ++ [x] ++ 
			   qsort (filter (>= x) xs)

-- qch1 = quickCheck(\xs -> length (qsort xs) == length xs)

qch2 = quickCheck((\xs -> length (qsort xs) == length xs)::[Int]->Bool)
qch3 = quickCheck((\xs -> qsort xs == sort xs)::[Int]->Bool)

qch4 = quickCheck((\xs -> qsort(qsort xs) == qsort xs)::[Int]->Bool)

isSorted :: Ord a => [a] -> Bool
isSorted xs = sort xs == xs

isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' xs = and $ zipWith (<=) (init xs) (tail xs)

qch5 = quickCheck((\xs -> isSorted (qsort xs))::[Int]->Bool)

qch6 = quickCheck((\xs -> isSorted' (qsort xs))::[Int]->Bool)

------------------------------------------------------