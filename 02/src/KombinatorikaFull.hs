module Kombinatorika where

import Test.QuickCheck
import Data.List

fact :: Int -> Int
fact n = product [1..n]

comb :: Int -> Int -> Int
comb n k = (fact n) `div` ((fact k) * (fact (n-k)))

-- permutacie 
perms :: [t] -> [[t]]
perms [] = [[]]
perms (x:xs) = [insertInto x i ys | ys <- perms xs, i <- [0..length ys]]
               where
               insertInto x i xs = (take i xs) ++ (x:drop i xs)

qch1 = quickCheck(\n -> (n > 0 && n < 10) ==> length (perms [1..n]) == fact n)

-- kombinacie bez opakovania
kbo  :: [t] -> Int -> [[t]]
kbo xs k  | k == 0             = [[]]
         | k == (length xs)   = [xs]
kbo (x:xs) k                  = [x:komb | komb <- kbo xs (k-1)] ++ kbo xs k

qch2 = quickCheck(\n -> \k -> (n > 0 && n < 15 && k >= 0 && k <= n) ==> 
                              length (kbo [1..n] k) == comb n k)

-- kombinacie s opakovanim
kso  :: [t] -> Int -> [[t]]
kso _ 0                       = [[]]
kso [] k                      = []
kso (x:xs) k                  = [x:komb | komb <- kso (x:xs) (k-1)] ++ kso xs k

qch3 = quickCheck(\n -> \k -> (n > 0 && n < 10 && k >= 0 && k <= 3+n) ==> 
                              length (kso [1..n] k) == comb (n+k-1) (k))

-- variacie bez opakovania
vbo  :: (Eq t) => [t] -> Int -> [[t]]
vbo xs k | k == 0             = [[]]
         | k > (length xs)    = []
vbo xs k                      = [ x:var  | x <- xs, var <- vbo (xs\\[x]) (k-1) ]

qch4 = quickCheck(\n -> \k -> (n > 0 && n < 10 && k >= 0 && k <= n) ==> 
                              length (vbo [1..n] k) == product [n,n-1..(n-k+1)])


-- variacie s opakovanim
vso  :: [t] -> Int -> [[t]]
vso xs 0    = [[]]
vso xs k    = [ x:var  | x <- xs, var <- vso xs (k-1) ]

qch5 = quickCheck(\n -> \k -> (n > 0 && n < 10 && k >= 0 && k <= 2*n) ==> 
                              length (vso [1..n] k) == product (take k (repeat n)))

