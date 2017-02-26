module MaxSucet where

import Data.List
import Data.Ord

maxSucet :: [Int] -> (Int, [Int])
maxSucet list = if partialResult == [] || fst maxSumPair < 0 then (0, [])
                else maxSumPair
                where 
                    slice index size = [list !! sliceIndex | sliceIndex <- [index..index+size-1]]
                    partialResult = 
                        [ (sum (slice index size), slice index size)
                        | size<-[1..length list], index <- [0..length list - size]]
                    maxSumPair = maximumBy (comparing fst) partialResult
