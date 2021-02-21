module Main where

import qualified Primes as F
import Test.HUnit
import System.Random

main = do
	g <- getStdGen
	runTestTT $ TestList [ let lst = map (`mod` 66666) $ take 100 (randoms g :: [Integer]) in TestList [ TestCase $ assertEqual ("take" ++ (show lst) ++ " primes") (take 10 myprimes) (take 10 F.primes) | x <- lst ] ]
    
-- riesenie tutora
myprimes  :: [Integer]
myprimes = 0:sieve [2..]  where 
             sieve (0:xs) = 0 : sieve xs
             sieve (x:xs) = x : sieve [ if y `mod` x == 0 then 0 else y | y<-xs]
             sieve [] = []		