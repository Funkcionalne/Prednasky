module Knight where

-- zdroj : http://learnyouahaskell.com/a-fistful-of-monads#the-list-monad
import Control.Monad
import Data.List

type KnightPos = (Int,Int) 

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
                      (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
                                  ,
                                  (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
                           ]  
                      guard (c' `elem` [1..8] && r' `elem` [1..8])  
                      return (c',r') 

in2 :: KnightPos -> [KnightPos]  
in2 start = do   
              first <- moveKnight start  
              second <- moveKnight first
              return second
                      
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
              first <- moveKnight start  
              second <- moveKnight first  
              moveKnight second   
    
ink :: Int -> KnightPos -> [KnightPos]  
ink 0 start = return start
ink k start = do   
                m <- moveKnight start  
                mm <- ink (k-1) m
                return mm
    
    
moveKnight' :: KnightPos -> [KnightPos]  
moveKnight' (c,r) = filter onBoard  
                        [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
                        ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
                        ]  
                  where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]      