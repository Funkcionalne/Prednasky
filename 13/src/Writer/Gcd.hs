module Gcd where
import Control.Monad.Writer 
 
out :: Int -> Writer [String] Int  
out x = writer (x, ["number: " ++ show x])  

{-
out 4
runWriter $ out 4
execWriter $ out 4
-}
  
mult :: Writer [String] Int  
mult = do { a <- out 3; b <- out 5; return (a*b)}

{-
mult
runWriter mult
execWriter mult
-}
    
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  | b == 0 = -- do tell ["result " ++ show a]  
                     --   return a  
                     writer (a, ["result " ++ show a])
          | otherwise = do tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
                           gcd' b (a `mod` b) 
                        --do let modulo = (a `mod` b)
                        --   result <- gcd' b modulo
                        --   writer (result, [show a ++ " mod " ++ show b ++ " = " ++ show modulo])
                          
{-
"?: " gcd' 18 12
WriterT (Identity (6,["18 mod 12 = 6","12 mod 6 = 0","result 6"]))

runWriter (gcd' 2016 48)
(48,["2016 mod 48 = 0","result 48"])

fst $ runWriter (gcd' 2016 48)
map putStr (snd $ runWriter (gcd' 2016 48))

mapM_ putStrLn (snd $ runWriter (gcd' 2016 48))
mapM putStrLn (snd $ runWriter (gcd' 2016 48))
2016 mod 48 = 0
result 48
[(),()]

mapM_ putStrLn (snd $ runWriter (gcd' 2016 48))
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