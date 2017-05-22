module Eval where

data Term = Con Int | Div Term Term deriving(Show, Read, Eq)

eval          :: Term -> Maybe Int
eval(Con a)   = return a
eval(Div t u) =   do    valT <- eval t 
                        valU <- eval u
                        if valU == 0 then
                          Nothing
                        else
                          return (valT `div` valU)
                            
e1 = eval (Div (Div (Con 1972) (Con 2)) (Con 23))
e2 = eval (Div(Con 1)(Con 0))
e3 = eval (Div (Div (Div (Con 1972) (Con 2)) (Con 23)) (Div(Con 1)(Con 0)))
