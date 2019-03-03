module Tree where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)


import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Data.Map hiding (map,null,filter,foldr)
import Data.List (nub,delete)
import Data.Data
import Data.Char
import Control.Monad
import Control.Monad.State
import Data.Maybe (maybeToList)

data BVS t = Nil | Node (BVS t) t (BVS t)  
      deriving(Show, Eq)
      
{-	  
gener  :: Int -> BVS Int
gener 0     = Nil
gener depth = Node (gener (depth - 1)) 5 (gener (depth - 1))
		 where nextInt = generate (arbitrary::Gen Int)
		 
gener'  :: Int -> BVS Int
gener' 0     = Nil
gener' depth = do  
				  nextInt <- generate (arbitrary::Gen Int)
				  return Node (gener' (depth - 1)) 5 (gener' (depth - 1))
		 
-}

instance Arbitrary a => Arbitrary (BVS a) where
  arbitrary = frequency 
              [
                (1, return Nil )
              , (1, liftM3 Node arbitrary arbitrary arbitrary)
              ]
-- generate (arbitrary::Gen (BVS Int))
              
isBVS    :: (Ord t) => BVS t -> Bool
isBVS Nil = True
isBVS (Node left value right) = 
      (all (<value) (flat left))
      &&
      (all (>value) (flat right))
      &&
      isBVS left
      &&
      isBVS right
   
find  :: (Ord t) => t -> (BVS t) -> Bool
find _ Nil = False
find x (Node left value right)  | x == value = True
--                                | x < value = find x left
--                                | x > value = find x right
                                | x < value = find x right
                                | x > value = find x left
{--
*** Failed! Falsifiable (after 2 tests): 
0
Node (Node Nil (-1) (Node Nil 0 Nil)) 1 Nil
--}                                

flat  :: BVS t -> [t]                                
flat Nil = []
flat (Node left value right)  = flat left ++ [value] ++ flat right

e  = Node Nil 4 (Node Nil 7 Nil)

qch1 = verbose((\x -> \tree -> find x tree)::Int->(BVS Int)->Bool)
qch2 = quickCheck((\x -> \tree -> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Bool)
{--
"?: " qch2
*** Failed! Falsifiable (after 3 tests): 
1
Node Nil (-2) (Node Nil 1 Nil)
(0.00 secs, 0 bytes)
--}
qch3 = quickCheck((\x -> \tree -> (isBVS tree) ==> ((find x tree) == (elem x (flat tree))))::Int->BVS Int->Property)

{--
"?: " qch3
*** Failed! Falsifiable (after 13 tests): 
0
Node (Node (Node Nil (-5) Nil) 2 (Node (Node Nil 10 Nil) 0 Nil)) 11 Nil
(0.00 secs, 0 bytes)

--}