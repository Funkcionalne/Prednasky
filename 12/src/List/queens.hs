module Queens where

import Control.Monad


check (i,j) (m,n) = (i==m) || (j==n) || (j+i==n+m) || (j+m==i+n)

safe p n = all (True==) 
               [not (check (i,j) (m+1,n)) | (i,j) <- zip [1..m] p]
           where m=length p
     
queens n = queens1 n n
queens1 n v   | n==0 = [[]]
              | otherwise = [y++[p] | y <- queens1 (n-1) v, p <- [1..v], safe y p]

mqueens n = mqueens1 n n
mqueens1 n v  | n==0 = return []
              | otherwise =   do
                                y <- mqueens1 (n-1) v
                                p <- [1..v]
                                guard (safe y p)
                                return (y++[p])
              
              
