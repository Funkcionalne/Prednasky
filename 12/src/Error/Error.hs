module Error where

{-
instance (Error e) => Monad (Either e) where  
        return x = Right x   
        Right x >>= f = f x  
        Left err >>= f = Left err  
        fail msg = Left (strMsg msg)  
-}

data Term = Con Int | Div Term Term deriving(Show, Read, Eq)

eval          :: Term -> Either String Int 
eval(Con a)   = return a
eval(Div t u) = do  
                    valT <- eval t 
                    valU <- eval u
                    if valU == 0 then
                        fail "div by zero"
                    else  
                        return (valT `div` valU)
 
t :: Term
t = Div (Con 1972) (Con 23) 

t' :: Term
t' = Div (Con 1972) (Con 0) 

{-
"?: " eval t
Right 85
"?: " eval t'
*** Exception: div by zero
-}
