module Arrays where
import Test.QuickCheck

set :: [t] -> Int -> t -> [t]
set xs i value | i < 0           = xs    -- out of range
               | i >= length xs  = xs    -- out of range
               | otherwise       = (if i == 0 then value else y):set ys (i-1) value
                                   where (y:ys) = xs
                                   
set' :: [t] -> Int -> t -> [t]
set' xs i value | i < 0          = xs    -- out of range
                | i >= length xs = xs    -- out of range
                | otherwise      = let (y:ys) = xs in
                                    (if i == 0 then value else y):set' ys (i-1) value
                                   
set'' :: [t] -> Int -> t -> [t]
set'' xs i value | i < 0             = xs    -- out of range
                 | i >= length xs    = xs    -- out of range
                 | otherwise         = [xs!!j | j <- [0.. i-1] ] 
                                       ++ [value] ++ 
                                       [xs!!j | j <- [i+1..length xs-1] ]
                                   
qch1 = quickCheck( (\xs -> \i -> \value -> set xs i value == set' xs i value)::[Int]->Int->Int->Bool)                                    
qch2 = quickCheck( (\xs -> \i -> \value -> set xs i value == set'' xs i value)::[Int]->Int->Int->Bool)                                    