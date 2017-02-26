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
              