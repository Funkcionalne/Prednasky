import Data.Char
import Control.Monad.State

data Tree a = 	Nil | 
                Node a (Tree a) (Tree a) 
                deriving (Show, Eq)

preorder :: Tree a -> State [a] ()
preorder Nil 	= return ()
preorder (Node value left right) = 
                    do 
                        str <-get  -- hodnota stavu
                        put (value:str)  -- prepis hodnotu stavu
                        preorder left
                        preorder right
                        return ()
                        
e :: Tree String
e = Node "c" (Node "a" Nil Nil) (Node "b" Nil Nil)
e' :: Tree String
e' = Node "d" e e

-- execState (preorder e) []
-- evalState (preorder e) []

--------------------------------------

reindex :: Tree a -> State Int (Tree Int)
reindex Nil 	= return Nil
reindex (Node value left right) = do 
                                      i <- get    -- hodnota stavu
                                      put (i+1)   -- prepis hodnotu stavu
                                      ileft <- reindex left
                                      iright <- reindex right
                                      return (Node i ileft iright) 

-- evalState (reindex e') 0
-- execState (reindex e') 0

----------------------------------------------------

type Table a = [a]
numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)

numberTree Nil = return Nil
numberTree (Node x t1 t2) 
        =  do num <- numberNode x
              nt1 <- numberTree t1
              nt2 <- numberTree t2
              return (Node num nt1 nt2)
     where 
     numberNode :: Eq a => a -> State (Table a) Int
     numberNode x = do table <- get
                       (newTable, newPos) <- return (add2Table x table)
                       put newTable
                       return newPos
     add2Table::  (Eq a) => a -> Table a -> (Table a, Int)
     add2Table x table = case (findIndexInList (== x) table) of
                          Nothing -> (table ++ [x], length table)
                          Just i  -> (table, i)
     findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
     findIndexInList = findIndexInListHelp 0
     findIndexInListHelp _ _ [] = Nothing
     findIndexInListHelp count f (h:t) = if (f h)
                                            then Just count
                                            else findIndexInListHelp (count+1) f t
--
-- numTree applies numberTree with an initial state:
--
numTree :: (Eq a) => Tree a -> Tree Int
numTree t = evalState (numberTree t) []
--
testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil
--numTree testTree => Node 0 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil
