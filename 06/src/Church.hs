{-# LANGUAGE RankNTypes #-}
module Church where

true  x y = x
false x y = y

ifte  c t e = c t e
-------------
two   f x = f (f x)
one   f x = f x
zero  f x = x
-------------

incr n f x = f (n f x)

add   m n f x = m f (n f x)
mul   m n f x = m (n f) x
-------------
isZero n =  n (\_ -> false) true

decr n = n (\m f x -> f (m incr zero))
           zero
           (\x -> x)
           zero
-------------
fact :: (forall a. (a->a)->a->a) -> (a->a) -> a -> a        
fact n  =
     ifte (isZero n)
          one
          (mul n (fact (decr n)))

main = 
  -- print $ (add (mul two two) one) (+1) 0  
  -- print $ (decr (add (mul two two) one)) (+1) 0  
  -- print $ (fact (add (mul two two) one)) (+1) 0
  print $ (fact (add two (add (mul two two) (mul two two)))) (+1) 0
  
-- 3628800
-- (4.75 secs, 2,598,673,208 bytes)
  