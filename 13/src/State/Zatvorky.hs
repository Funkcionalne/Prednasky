module StateGame where

import Control.Monad.State

type Value = Int
type Stav = (Bool, Int)

loop :: String -> State Stav Value
loop []     = do (ok, depth) <- get
                 if ok then return depth else return (-1)

loop (x:xs) = do
    (ok, depth) <- get
    case x of
         '(' | ok -> put (ok, depth + 1)
         ')' | ok -> if depth > 0 then put (ok, depth - 1) else put (False, 999)
    loop xs

main = print $ 0 == evalState (loop "(()") (True, 0)
