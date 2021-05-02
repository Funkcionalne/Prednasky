module Writer where
import Control.Monad.Writer 

{-
newtype Writer w a = Writer { runWriter :: (a, w) }  

import Data.Monoid 
instance (Monoid w) => Monad (Writer w) where  
   return x = Writer (x, mempty)  
   (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  
  -} 
-------------------    
data Term = Con Int | Div Term Term deriving(Show, Read, Eq)

line        :: Term -> Int -> String
line t a    = "eval (" ++ show t ++ ") <=" ++ show a ++ "\n"

eval          :: Term -> Writer String Int
eval x@(Con a)   = --do tell (line x a)
                   --   return a
                   writer (a, line x a)
eval x@(Div t u) = do  
                        valT <- eval t 
                        valU <- eval u
--                        tell (line x (valT `div` valU))
--                        return (valT `div` valU)
                        let result = (valT `div` valU)
                        writer (result, (line x result))
 
t :: Term
t = (Div (Div (Con 1972) (Con 2)) (Con 23))
{-
execWriter (eval t)
"eval (Con 1972) <=1972\neval (Con 2) <=2\neval (Div (Con 1972) (Con 2)) <=986\neval (Con 23) <=23\neval (Div (Div (Con 1972) (Con 2)) (Con 23)) <=42\n"

putStr $ execWriter (eval t)
eval (Con 1972) <=1972
eval (Con 2) <=2
eval (Div (Con 1972) (Con 2)) <=986
eval (Con 23) <=23
eval (Div (Div (Con 1972) (Con 2)) (Con 23)) <=42
-}


-- tell :: MonadWriter w m => w -> m ()

