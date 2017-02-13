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
{--
Tvrdenie:
decr zero f x = zero
decr n f x = f^(n-1) x
Dokaz
zero (\m f x -> f (m incr zero)) zero id zero = zero id zero = zero
one (\m f x -> f (m incr zero)) zero id zero = 
  (\m f x -> f (m incr zero)) zero id zero =
  (f x -> f (zero incr zero)) id zero =
  (x -> id (zero incr zero)) zero =
  (id (zero incr zero)) =
  (zero incr zero) =
  zero
indukcia:
(incr N) (\m f x -> f (m incr zero)) zero id zero = 
  (\f x -> f (N f x)) (\m f x -> f (m incr zero)) zero id zero = 
  (\m f x -> f (m incr zero)) (N (\m g y -> g (m incr zero)) zero) id zero =   ind.hyp.
  (\m f x -> f (m incr zero)) (g^(n-1) y) id zero =  
  (\f x -> f ((f^(n-1) y) incr zero))  id zero =  
  (\x -> ((f^(n-1) x) incr zero))  zero =  
  ((f^(n-1) x) incr zero) =  ????

--}

-------------
fact :: (forall a. (a->a)->a->a) -> (a->a) -> a -> a        
fact n  =
     ifte (isZero n)
          one
          (mul n (fact (decr n)))

main = 
  -- print $ (decr (add (mul two two) one)) (+1) 0  
  -- print $ (fact (add (mul two two) one)) (+1) 0
  print $ (fact (add two (add (mul two two) (mul two two)))) (+1) 0
  
-- 3628800
-- (4.75 secs, 2,598,673,208 bytes)
  