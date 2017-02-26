module Luhn where
import Data.Char

cardnumber :: Integer -> Bool
cardnumber s = (sum $ map (
                      \zoznam12 -> case zoznam12 of
                          [a,b] -> a + (if (b * 2 > 9) then b * 2 - 9 else b * 2)
                          [a] -> a
                    ) $ splitEvery 2 split ) `mod` 10 == 0
     where split = map (\x -> ord x - ord '0') $ reverse $ show s
  
-- Data.List.Grouping
-- | partitions list into sub-lists of length given by the Int:
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
                  where (as,bs) = splitAt n xs  
     
main = print [
      cardnumber(49927398716),
      cardnumber(49927398717),
      cardnumber(1234567812345678),
      cardnumber(1234567812345670)]