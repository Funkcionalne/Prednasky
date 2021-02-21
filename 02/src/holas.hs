module Holas where
import Data.List(sortBy)

holas :: [Int] -> [Int] -> [(Int, Int, Int)]
holas xs ys = allxs
				where 
				xs' = addIndexes xs
				ys' = addIndexes ys
				sxs = sortBy cmp xs'
				sys = sortBy cmp ys'
				allxs = alls sxs sxs
				allys = alls sys sys

alls1 :: [Int] ->[(Int, Int, Int)]				
alls1 xs = alls sxs sxs
				where
				xs' = addIndexes xs
				sxs = sortBy cmp xs'
				
				
alls :: [(Int, Int)] -> [(Int, Int)] ->[(Int, Int, Int)]				
alls [] _ = []
alls _ [] = []
alls allxs@((x,i):xs) allys@((y,j):ys) =  sortBy cmp3 sss
						where 
						   sss = filter (\(_,ii,jj) -> (ii /= jj) && not((i==ii) && (j==jj)))
								  [ let tupx = (allxs!!dx); 
								        tupy = (allys!!dy) in
								       ((fst tupx) * (fst tupy), snd tupx, snd tupx)
								    | dx <- [0..2], dy <- [0..2]
							      ]
				
cmp	:: (Int, Int) -> (Int, Int) -> Ordering
cmp (x, _) (y, _) | x < y = LT
				  | x > y = GT
				  | otherwise = EQ

cmp3	:: (Int, Int, Int) -> (Int, Int, Int) -> Ordering
cmp3 (x, _, _) (y, _, _) | x < y = LT
						 | x > y = GT
						 | otherwise = EQ

addIndexes	:: [Int] -> [(Int,Int)]
addIndexes xs = [ (xs!!i,i) | i<- [0..(length xs)-1] ]