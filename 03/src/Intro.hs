module Intro where

foo           :: [Integer] -> Integer
foo []        = 0
foo (x:xs)    | odd x      = (3*x + 1) + foo xs
              | otherwise = foo xs

foo'  xs  = sum [ 3*x+1 | x <- xs, odd x]           
foo''  xs  = sum (map (\x -> 3*x+1) ( filter (odd) xs))
foo''' xs  = sum $ map (\x -> 3*x+1) $ filter (odd) xs
foo''''  = sum . map (\x -> 3*x+1) . filter (odd)
foo'''''  = sum . map ((+1).(*3)) . filter (odd)
foo''''''  = foldr (+) 0 . map ((+1).(*3)) . filter (odd)

-----------------------------------

goo   :: [Integer] -> Integer
goo   []      = 1
goo   (x:xs)  
  | even x    = (x-2) * goo xs
  | otherwise = goo xs
  
goo' xs = product [ x-2 | x <- xs, even x]           
goo''  = product . map (subtract 2) . filter (even)
goo'''  = foldl (*) 1 . map (subtract 2) . filter (even)

---------------------
hoo :: Integer -> [Integer]
hoo 1 = []
hoo n | even n  = n : hoo (n `div` 2)
      | otherwise = n : hoo (3 * n + 1)

hoo'  = takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1 )      
      
---------------------------------------

cifry :: Integer -> [Integer]
cifry n = map(`mod` 10) $ reverse $ takeWhile (> 0) $ iterate (`div`10) n
 
cifryR :: Integer -> [Integer] 
cifryR n = map(`mod` 10) $ takeWhile (> 0) $ iterate (`div`10) n

cifry' :: Integer -> [Integer]
cifry' = map(`mod` 10) . reverse . takeWhile (> 0) . iterate (`div`10)
      
cifryR' :: Integer -> [Integer] 
cifryR' = map(`mod` 10) . takeWhile (> 0) . iterate (`div`10)
      
---------------------------

moo :: Integer -> Integer
moo 1 = 0
moo n | even n  = n + moo (n `div` 2)
      | otherwise = moo (3 * n + 1)

moo' n = 2+(snd $ last $ takeWhile ((/=1).fst) $ iterate (\(x,s) -> if even x then (x `div` 2,x+s) else (3 * x + 1,s) ) (n,0))

moo'' n = snd $ last $ takeWhile ((/=1).fst) $ iterate (\(x,s) -> if even x then (x `div` 2,x+s) else (3 * x + 1,s) ) (n,2)

------------------

maxSucet' :: [Int] -> Int
maxSucet' [] = 0
maxSucet' xs = maximum (map (maximum) (init (foldl (\t -> \p -> (p:(map (+p) (head t))):t) [[]] xs)))

maxSucet'' :: [Int] -> Int
maxSucet'' [] = 0
maxSucet'' xs = maximum $ map (maximum) $ init $ foldl (\t -> \p -> (p:(map (+p) (head t))):t) [[]] xs 

maxSucet''' :: [Int] -> Int
maxSucet''' = maximum . map (maximum) . init . foldl (\t -> \p -> (p:(map (+p) (head t))):t) [[]] 

--------------
-- https://gist.github.com/dapenggao/dff20642bc0db606f6b6
kadane :: [Int] -> Int -> Int -> Int -- list -> tempMax -> globalMax -> max
kadane []     _       globalMax = globalMax
kadane (x:xs) tempMax globalMax = kadane xs newTempMax newGlobalMax
	where
		newTempMax = max (tempMax + x) 0
		newGlobalMax = max globalMax newTempMax

------------------------

kadane' :: [Int] -> Int 
kadane' (x:xs) = snd $ foldr f (0,0) xs
    where f x (tempMax, globalMax) = let newTempMax = max (tempMax + x) 0 
            in (newTempMax, max globalMax newTempMax)

------------------------

kadane'' :: [Int] -> Int 
kadane'' = snd . foldr f (0,0)
    where f x (tempMax, globalMax) = let newTempMax = max (tempMax + x) 0 
            in (newTempMax, max globalMax newTempMax)
            
            
{-            
"?: " maxSucet' [(-1), 2, 1, (-3), 2, 3, 1]
6
"?: " maxSucet'' [(-1), 2, 1, (-3), 2, 3, 1]
6
"?: " kadane [(-1), 2, 1, (-3), 2, 3, 1] 0 0
6
"?: " kadane' [(-1), 2, 1, (-3), 2, 3, 1]
6
"?: " kadane'' [(-1), 2, 1, (-3), 2, 3, 1]
6
-}