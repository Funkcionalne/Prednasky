module Generatory where

import Test.QuickCheck
import Text.Show.Functions
import Data.List(sort)
import Test.QuickCheck.Arbitrary
import Control.Monad


kocka :: Gen Int
kocka = choose(1,6)
-- generate kocka

yesno :: Gen Bool
yesno = choose(True, False)
-- generate yesno

data  Colour = Red | Blue | Green

instance Arbitrary Colour where 
  arbitrary = oneof 
    [ return Red, return Blue, return Green]

{--
instance Arbitrary a => Arbitrary [a] where
      arbitrary = oneof
        [ return [], liftM2 (:) arbitrary arbitrary]
--}
    
data  Minca = Hlava | Panna deriving (Show)
instance Arbitrary Minca where
  arbitrary = oneof [return Hlava, return Panna]

-- generate (arbitrary::Gen Minca)
        
falosnaMinca :: Gen Minca
falosnaMinca = frequency [(1,return Hlava), (2,return Panna)]
-- generate falosnaMinca

------------------------------------- zoznamy

arbitraryListMax8Len :: Arbitrary a => Gen [a]
arbitraryListMax8Len =
    do
      k <- choose (0, 8)::(Gen Int)
      sequence [ arbitrary | _ <- [1..k] ]

mysized :: (Int -> Gen a) -> Gen a
mysized f = f 50


arbitraryList :: Arbitrary a => Gen [a]
arbitraryList =
  mysized ( \n -> do
                  k <- choose (0, n)
                  sequence [ arbitrary | _ <- [1..k] ] )

{--
"?: " generate (arbitraryList::Gen [Int])
[-9,7,14,24,18,28,-4,0,22,12,-14]
"?: " generate (arbitraryList::Gen [Int])
[-2,20,-28,15,-1,-25,-28,-1,30,-12,15,9,6,-19,-2,23,20,12,12,-21,4,-6]
--}

data Tree t = Leaf t | Node (Tree t) t (Tree t)  
      deriving (Show, Ord, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency 
              [
                (1, liftM Leaf arbitrary )
              , (1, liftM3 Node arbitrary arbitrary arbitrary)
              ]
-- generate (arbitrary::Gen (Tree Int))
              
strom :: Gen (Tree Int)
strom = frequency 
              [
                (1, liftM Leaf arbitrary )
              , (10, liftM3 Node arbitrary arbitrary arbitrary)
              ]
              
strom' :: Gen (Tree Int)
strom' = frequency 
              [
                (1, liftM Leaf arbitrary )
              , (3, liftM3 Node arbitrary arbitrary arbitrary)
              ]
              
strom'' :: Gen (Tree Int)
strom'' = frequency 
              [
                (1, liftM Leaf arbitrary )
              , (50, liftM3 Node arbitrary arbitrary arbitrary)
              ]
              
strom''' :: Gen (Tree Int)
strom''' = frequency 
              [
                (20, liftM Leaf arbitrary )
              , (1, liftM3 Node arbitrary arbitrary arbitrary)
              ]
              