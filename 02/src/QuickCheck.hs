module QCH where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)


ch1   = 
      quickCheck(\(xs,n) -> length (take n xs) == n)

ch2   = 
      verboseCheck (\(xs,n) -> length (take n xs) == n)

ch3   = 
      verboseCheck (\(xs,n) -> n>=0 ==> length (take n xs) == n)

ch4   = 
      quickCheck (\(xs,n) -> n>=0 && length xs >= n ==> length (take n xs) == n)

      
x1 =  
      quickCheck (\(m,n) -> length [m..n] == n-m+1)
-- verboseCheck (\(m,n) -> length [m..n] == n-m+1)
      
x2 =  
      quickCheck (\(m,n) -> m <= n ==> length [m..n] == n-m+1)

x3 =
      quickCheck((\xs -> (length (reverse xs ) == length xs )))
      
x4 =
      quickCheck((\xs -> \ys -> ( (xs, ys) ==  unzip (zip xs ys)  )))      
      
x5  =
      quickCheck((\xs -> \ys -> ( length xs == length ys ==>
              (xs, ys) ==  unzip (zip xs ys)  )))      
{-
ch5   = 
      quickCheck ( (\xs -> \p1 -> \p2 -> 
            filter p1 (filter p2 xs) == filter (p1 && p2) xs)
                  :: [Int] -> (Int->Bool) -> (Int->Bool) -> Bool)
-}  
  
ch6   =       
      quickCheck ( (\xs -> \p1 -> \p2 -> 
            filter p1 (filter p2 xs) == filter (\x -> (p1 x) && (p2 x)) xs)
                  :: [Int] -> (Int->Bool) -> (Int->Bool) -> Bool)

ch7   =       
      quickCheck ( (\xs -> \p1 -> \p2 -> 
            (filter p1 xs) ++ (filter p2 xs) == filter (\x -> p1 x || p2 x) xs)
                  :: [Int] -> (Int->Bool) -> (Int->Bool) -> Bool)


test1 = quickCheck((\xs -> \ys -> (length (xs ++ ys) == length xs + length ys)))
test2 = quickCheck((\xs -> (length (reverse xs ) == length xs )))
test3 = quickCheck((\xs -> \ys -> ( (xs, ys) ==  unzip (zip xs ys)  )))
test4 = quickCheck((\xs -> \ys -> if length xs == length ys then ( (xs, ys) ==  unzip (zip xs ys)  ) else True) :: ([Int] -> [Int] -> Bool))
test5 = quickCheck( (\f -> \xs -> \ys -> ((map f xs) ++ (map f ys) == map f (xs ++ ys))) :: (Int->Int) ->[Int] -> [Int] -> Bool)
test6 = quickCheck( (\xs -> \ys -> ((map id xs) ++ (map id ys) == map id (xs ++ ys))) :: [Int] -> [Int] -> Bool)

--

comutat = 
  quickCheck(
            (\x -> \f -> \g -> (f.g) x == (g.f) x)::Int->(Int->Int)->(Int->Int)->Bool)

asociat = 
  quickCheck(
            (\x -> \f -> \g -> \h -> (f.(g.h)) x == ((f.g).h) x)
            ::Int->(Int->Int)->(Int->Int)->(Int->Int)->Bool)

-- map id xs = xs			    map id = id
map_id = 
  quickCheck(
            (\xs -> map id xs == xs) ::[Int]->Bool)
            
            
            
-- map (f.g) xs = map f (map g xs)		    map f . map g = map (f.g)
map_f_g = 
  quickCheck(
            (\xs -> \f -> \g -> map (f.g) xs == map f (map g xs))
            ::[Int]->(Int->Int)->(Int->Int)->Bool)

-- head (map f xs) = f (head xs)	                 head . map f = f . head            
head_map_f = 
  quickCheck(
            (\xs -> \f -> head (map f xs) == f (head xs))
            ::[Int]->(Int->Int)->Bool)

head_map_f' = 
  quickCheck(
            (\xs -> \f -> if (length xs > 0) then head (map f xs) == f (head xs) else True)
            ::[Int]->(Int->Int)->Bool)

            
-- tail (map f xs) = map f (tail xs)	                 tail . map f = map f . tail            
tail_map_f = 
  quickCheck(
            (\xs -> \f -> tail (map f xs) == map f (tail xs))
            ::[Int]->(Int->Int)->Bool)

tail_map_f' = 
  quickCheck(
            (\xs -> \f -> if (length xs > 0) then (tail (map f xs) == map f (tail xs)) else True)
            ::[Int]->(Int->Int)->Bool)

-- map f (xs++ys) = map f xs++map f ys
map_f_append =       
    quickCheck( (\xs -> \ys -> \f -> ((map f xs) ++ (map f ys) == map f (xs ++ ys))) 
    :: [Int] -> [Int] -> (Int->Int) -> Bool)
            
-- length (map f xs) = length xs	                 length . map f = length
map_f_length =       
    quickCheck( (\xs -> \f -> (length (map f xs) == length xs)) 
    :: [Int] -> (Int->Int) -> Bool)
            
-- map f (reverse xs) = reverse (map f xs)	    map f.reverse=reverse.map f 
map_f_reverse =       
    quickCheck( (\xs -> \f -> (map f (reverse xs) == reverse (map f xs))) 
    :: [Int] -> (Int->Int) -> Bool)

-- sort (map f xs) = map f (sort xs)		    sort . map f = map f . sort
map_f_sort =       
    quickCheck( (\xs -> \f -> (sort (map f xs) == map f (sort xs))) 
    :: [Int] -> (Int->Int) -> Bool)

-- map f (concat xss) = concat (map (map f) xss)
map_f_concat =       
    quickCheck( (\xss -> \f -> (map f (concat xss) == concat (map (map f) xss))) 
    :: [[Int]] -> (Int->Int) -> Bool)

-- filter p (map f xs) 	= map f (filter  (p.f) xs)	
map_filter_map =       
  quickCheck( (\xs -> \f -> \p -> (filter p (map f xs) == map  f (filter  (p.f) xs)))
    :: [Int] -> (Int->Int) -> (Int->Bool) -> Bool)

