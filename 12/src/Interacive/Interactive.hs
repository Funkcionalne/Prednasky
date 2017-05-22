module Interactive where

import System.IO
import Control.Exception
{-
IO a ako state monada
type IO a = World -> (a,World)

getChar :: IO Char
putChar :: Char -> IO ()
-}

-- nacita 3 znaky a vrati z nich urobeny retazec
read3 :: IO [Char]
read3 = do  x <- getChar
            y <- getChar
            z <- getChar
            return [x,y,z]

-- nacita 3 znaky, druhy zahodi a vrati 1 a 3 ako dvojicu            
read1_1 :: IO (Char,Char)
read1_1 = do  x <- getChar
              getChar
              y <- getChar
              return (x,y)          
             
-- toto samozrejme existuje a vola sa to getLine
readLine  :: IO String
--readLine  :: IO [Char]
readLine  = do  x <- getChar
                if x == '\n' then
                  return []
                else
                  do str <- readLine
                     return (x:str)
             
-- zretaz riadky az po prvy volny riadok
zlep    :: IO String
zlep    = do  str <- getLine
              if length str == 0 then
                return []
              else
                do  zlepenec <- zlep
                    return (str++zlepenec)
             
-- spocitaj cisla az po 0
scitaj    :: IO Int
scitaj    = do str <- getLine
               let cislo = read str :: Int
               if cislo == 0 then 
                  return 0
               else
                  do
                      zvysok <- scitaj
                      return (cislo + zvysok)
               
-- hadaj cislo
hadaj100   :: IO ()
hadaj100   =   hadaj 1 100

hadaj   :: Int -> Int -> IO ()
hadaj from to =   if from > to then
                      putStrLn "klamal si"
                  else if from == to then
                    putStrLn ("je to cislo " ++ (show from))
                  else
                    do  putStrLn ("je vacsie ako " ++  (show ((from + to) `div` 2)) ++ " [y/n]")
                        ch <- getLine
                        if ch == "y" then   
                            hadaj (((from + to) `div` 2) + 1) to
                         else
                            hadaj from ((from + to) `div` 2)
             
----------------------------------------------------- HRA OBESENEC
             
obesenec :: IO ()
obesenec = do word <- getHiddenWord
              putStrLn "Hadaj:"
              cyklus word
             
getHiddenWord :: IO String
getHiddenWord = do
                    putStrLn "Zadaj slovo:"
                    hFlush stdout     -- ticky
                    pass <- withEcho False getLine
                    putChar '\n'
                    return pass
              where
                      withEcho :: Bool -> IO a -> IO a
                      withEcho echo action = do
                                                old <- hGetEcho stdin   -- ticky
                                                bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action    -- ticky

cyklus :: String -> IO ()
cyklus word = do  putStr "?"
                  guess <- getLine
                  if guess == word then
                      putStrLn "Uhadol si"
                  else
                      do  putStrLn (porovnaj word guess)
                          cyklus word

porovnaj :: String -> String -> String
porovnaj xs ys = [if elem x ys then x else '-' | x <- xs]  

