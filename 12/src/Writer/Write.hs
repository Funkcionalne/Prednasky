module Write where
import Control.Monad.Writer 

{-
newtype Writer w a = Writer { runWriter :: (a, w) }  

import Data.Monoid 
instance (Monoid w) => Monad (Writer w) where  
   return x = Writer (x, mempty)  
   (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  
  -} 
  
out :: Int -> Writer [String] Int  
out x = writer (x, ["number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- out 3  
    b <- out 5  
    return (a*b)    
    
-------------------    
data Term = Con Int | Div Term Term deriving(Show, Read, Eq)

eval          :: Term -> Writer [String] Int 
eval(Con a)   = out a
eval(Div t u) = do  
                    valT <- eval t 
                    valU <- eval u
                    out (valT `div` valU)
                    return (valT `div` valU)
 
t :: Term
t = Div (Con 1972) (Con 23) 

-- tell :: MonadWriter w m => w -> m ()

gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  | b == 0 = do  
                          tell ["result " ++ show a]  
                          return a  
          | otherwise = do  
                          tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
                          gcd' b (a `mod` b) 
                          
{-
"?: " gcd' 18 12
WriterT (Identity (6,["18 mod 12 = 6","12 mod 6 = 0","result 6"]))

runWriter (gcd' 2016 48)
(48,["2016 mod 48 = 0","result 48"])

fst $ runWriter (gcd' 2016 48)
map putStr (snd $ runWriter (gcd' 2016 48))

"?: " mapM putStrLn (snd $ runWriter (gcd' 2016 48))
2016 mod 48 = 0
result 48
[(),()]

"?: " mapM_ putStrLn (snd $ runWriter (gcd' 2016 48))
2016 mod 48 = 0
result 48

-}                 

gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["result " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result  