--http://www.madandmoonly.com/doctormatt/mathematics/dice1.pdf
--http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
--http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf

module Probably where

import Fractions
import Control.Monad (liftM, ap)

data Prob a = States [(a, Fraction)] deriving Show

states :: Prob a -> [(a, Fraction)]
states (States s) = s

instance Monad Prob where
    (>>=) s f = States [(y, f_mul p1 p2) | (x, p1) <- states s, (y, p2) <- states (f x)]
    return x = States [(x, Frac 1 1)]

instance Functor Prob where
    fmap = liftM

instance Applicative Prob where
    pure  = return
    (<*>) = ap

norm :: Eq a => Prob a -> Prob a
norm = States . norm' . states

norm' [] = []
norm' ((x, p):ss) = (x, foldr f_add p (map snd $ filter ((==x).fst) ss)):(norm' (filter ((/=x).fst) ss))

uniform :: [a] -> Prob a
uniform xs = States [(x, p) | x <- xs] where p = Frac 1 (length xs)

dice = uniform [1..6]
