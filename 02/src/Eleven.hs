module Eleven where  
import Test.QuickCheck

oneStep :: Integer -> Integer
oneStep = \n -> abs $ uncurry (-) $ foldr (\c -> \(sp,sn) -> (c+sn, sp)) (0,0) $ map (`mod` 10) $ takeWhile (>0) $ iterate (`div` 10) n          

allSteps :: Integer -> Bool
allSteps = \n -> 0 == (head $ dropWhile (>9) $ iterate oneStep n)

qch1   =  quickCheck(\n -> (n>0) ==> allSteps n == (n `mod` 11 == 0))
