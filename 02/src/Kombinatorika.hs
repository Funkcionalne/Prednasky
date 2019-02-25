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
kbo = undefined

qch2 = undefined

-- kombinacie s opakovanim
kso  :: [t] -> Int -> [[t]]
kso = undefined

qch3 = undefined

-- variacie bez opakovania
vbo  :: (Eq t) => [t] -> Int -> [[t]]
vbo = undefined
qch4 = undefined

-- variacie s opakovanim
vso  :: [t] -> Int -> [[t]]
vso = undefined

qch5 = undefined
