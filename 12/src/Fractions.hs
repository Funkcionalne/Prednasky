module Fractions where

data Fraction = Frac Int Int

_gcd :: Int -> Int -> Int
_gcd x 0 = x
_gcd x y
    | x < y     = _gcd y x
    | otherwise = _gcd y (x `mod` y)

f_base :: Fraction -> Fraction
f_base (Frac x y) = Frac (x `div` n) (y `div` n) where n = _gcd x y

f_add :: Fraction -> Fraction -> Fraction
f_add (Frac x1 y1) (Frac x2 y2) = f_base (Frac (x1*y2 + x2*y1) (y1*y2))

f_mul :: Fraction -> Fraction -> Fraction
f_mul (Frac x1 y1) (Frac x2 y2) = f_base (Frac (x1*x2) (y1*y2))

instance Show Fraction where
    show f = (show x) ++ "/" ++ (show y) where (Frac x y) = f_base f
