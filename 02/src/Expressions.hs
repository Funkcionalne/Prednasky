module Expressions where

import Test.QuickCheck
import Text.Show.Functions
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
import Data.Map hiding (map,null,filter,foldr)
import Data.List (nub,delete)
import Data.Data
import Data.Char
import Control.Monad
import Control.Monad.State
import Data.Maybe (maybeToList)

data Op = Add | Sub | Mult | Div deriving(Eq, Show)
data Expr = Number Int | Operator Expr Op Expr  deriving(Eq, Show)
e = Operator (Number 2) Add (Number 4)

eval	:: Expr -> Int
eval (Number n)	= n
eval (Operator left Add right)	= eval left + eval right
eval (Operator left Sub right)	= eval left - eval right
eval (Operator left Mult right)	= eval left * eval right
eval (Operator left Div right)	= eval left `div` eval right


instance Arbitrary Op where
    --arbitrary = oneof [return Add, return Sub, return Mult, return Div]
	arbitrary     = do
		n <- choose (0, 3) :: Gen Int
		return $ case n of
                    0 -> Add
                    1 -> Sub
                    2 -> Mult
                    3 -> Div
	
-- generate (arbitrary::Gen Op)
	
randomOp :: IO Op
randomOp = generate (arbitrary::Gen Op)
	
instance Arbitrary(Expr) where
  arbitrary = frequency 
              [
                (1, liftM Number arbitrary )
              , (1, liftM3 Operator arbitrary arbitrary arbitrary)
              ]
		
randomExpr :: IO Expr
randomExpr = generate (arbitrary::Gen Expr)
