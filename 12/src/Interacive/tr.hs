-- zjednoduseny kod z All About Monads
import Control.Monad
import System.Environment
import System.IO

-- translate char in set1 to corresponding char in set2
tr :: String -> String -> Char -> Char
tr []     _      c = c
tr (x:xs) []     c = if x == c then ' ' else tr xs []  c
tr (x:xs) [y]    c = if x == c then  y  else tr xs [y] c
tr (x:xs) (y:ys) c = if x == c then  y  else tr xs ys  c

-- trs stdin to stdout based on commandline arguments
main :: IO ()
main = do [set1,set2] <- getArgs
          contents   <- hGetContents stdin
          putStr $ map (tr set1 set2) contents
 