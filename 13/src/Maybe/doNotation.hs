
import Data.Maybe
import Data.Either

fun1 :: String -> Maybe Int
fun1 [] = Nothing
fun1 s = Just $ length s

fun2 :: Int -> Maybe Float
fun2 r = if r < 0 then Nothing else Just $ sqrt (fromIntegral r)

fun3 :: Float -> Maybe Int
fun3 r = if ceiling r == floor r  then Just $ round r else Nothing

bind :: String -> Maybe Int
bind x = fun1 x >>= fun2 >>= fun3
bind'' x = fun1 x >>= (\i -> fun2 (i+1)) >>= (\f -> fun3 (3.14*f))

-- bind "abc" = Nothing
-- bind "abcd" = Just 2

doNotation x = do i <- fun1 x
                  f <- fun2 i
                  j <- fun3 f
                  return j
-- doNotation "abc" = Nothing
-- doNotation "abcd" = Just 2

doNotation' x = do i <- fun1 x
                   f <- fun2 i
                   fun3 f
                  
-- doNotation' "abc" = Nothing
-- doNotation' "abcd" = Just 2

doNotation'' x = do i <- fun1 x
                    f <- fun2 (i+1)
                    fun3 (3.14*f)

{-
instance Monad (Either a) where
  return r = Right r
  (Left l) >>= _ = Left l
  (Right r) >>= f = f r
-}  

efun1 :: String -> Either String Int
efun1 [] = Left "is empty"
efun1 s = Right $ length s

efun2 :: Int -> Either String Float
efun2 r = if r < 0 then Left "negative" else Right $ sqrt (fromIntegral r)

--efun3 :: Float -> Either String Int
--efun3 r = if ceiling r == floor r  then Right $ round r else Left "should be integer"

efun3 :: Float -> Either String [Int]
efun3 r = if ceiling r == floor r  then Right $ [ceiling r, floor r] else Left "should be integer"

--ebind :: String -> Either String Int
ebind :: String -> Either String [Int]
ebind x = efun1 x >>= efun2 >>= efun3
ebind'' x = efun1 x >>= (\i -> efun2 (i+1)) >>= (\f -> efun3 (3.14*f))

edoNotation'' x = do i <- efun1 x
                     f <- efun2 (i+1)
                     efun3 (3.14*f)
