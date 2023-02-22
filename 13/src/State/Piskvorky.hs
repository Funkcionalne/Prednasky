module Piskvorky where

import Control.Monad.State
import System.Random

size = 11
data PiskyState = PiskyState { playground :: [[Tile]], onTurn :: Bool, generator :: StdGen }
data Tile = Empty | X | O deriving (Eq, Show)

instance Show PiskyState where
  show pstate = concatMap (\row -> (concatMap tile row) ++ "\n") $ playground pstate
                where   tile :: Tile -> String
                        tile Empty= "."
                        tile X = "x"
                        tile O= "o"
-- player True/False
nextPlayer :: Bool->Bool
nextPlayer x = not x

-- player's sign
sign :: Bool -> Tile
sign True = X
sign False = O

-- no Empty on board
finish :: State PiskyState Bool
finish = do pstate <- get
            return $ any (==Empty) ( concat (playground pstate))
            
move :: State PiskyState (Int,Int)
move = do pstate <- get
          let free = [ (i,j) | i<-[0..size-1], j<-[0..size-1], (playground pstate)!!i!!j == Empty]
          let gen = generator pstate
          let (r, gen') = randomR (0, 120) gen
          --put $ pstate { generator = gen' }
          return $ free !! r
  
update :: (Int,Int) -> State PiskyState ()
update (row,col) = 
       do pstate <- get
          let player = onTurn pstate
          let s = sign player
          let newPlayground = [[ if i == row && j == col then s else (playground pstate)!!i!!j 
                                | j<-[0..size-1]] | i<-[0..size-1]]
          put $ pstate { onTurn = nextPlayer player, playground = newPlayground }  
          
oneTurn :: State PiskyState Bool
oneTurn = do (row, col) <- move
             update (row, col)
             finish
          
pinit :: StdGen -> PiskyState
pinit gen =  PiskyState (take size (repeat (take size ( repeat Empty))))
                      True
                      gen
main:: IO()
main = do g <- getStdGen
          let istate = pinit g
          putStr (show $ execState (sequence $ take 10 $ repeat oneTurn) istate)

