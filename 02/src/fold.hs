module Fold where

import Data.List

e1 = foldl (+) 0 [1..100]

e2 = foldl (\x y->10*x+y) 0 [1,2,3,4]

e3 = foldl (+) 0 [1..100]

e4 = foldl (\x y->10*x+y) 0 [1,2,3,4]

e5 = foldr max (-999) [1,2,3,4]

e6 = foldl max (-999) [1,2,3,4]

e7 = foldr (\_ -> \y ->(y+1)) 0 [3,2,1,2,4]

e8 = foldl (\x -> \_ ->(x+1)) 0 [3,2,1,2,4]

e9 = permutations [(+1),(+2),(*3), (^2)]

e10 = length $ permutations [(+1),(+2),(*3),(^2)]

apply :: [a -> b] -> [a] -> [b]
apply fs args = [ f a | f <- fs, a <- args]

e11 = maximum
        ( apply 
          (map (foldr (.) id) (permutations [(+1),(+2),(*3),(^2)]))
          [100]
        )
        
e12 = maximum $
        apply 
          (map (foldr (.) id) (permutations [(+1),(^2),(*3),(+2),(/3)]))
          [100]
                