module Automat where

import Control.Monad.State

-- Parkovaci automat
--Parkovaci automat sa zapína na signal '.', na začiatku je off. Keď je zapnutý, tak počíta počet automobilov, ktoré vošli '(' a počet automobilov, ktoré odišli ‚)‘. Stav automatu je ich rozdiel, výsledok výpočtu je počet automobilov, ktoré na parkovisku zostali, počas období, keď bol zapnutý

type Vysledok = Int
type Stav = (Bool, Int)

loop :: String -> State Stav Vysledok
loop []     = do (_, result) <- get
                 return result
loop (x:xs) = do (on, diff) <- get
                 case x of
                         '(' | on -> put (on, diff + 1)
                         ')' | on && diff >0 -> put (on, diff - 1)
                         '.'      -> put (not on, diff)
                         _        -> put (on, diff)
                 loop xs
                 
main :: IO ()
main = mapM_ print [ evalState (loop input) (False, 0) | input <- [
                                    "(()(.()()))((((.)(()",
                                    "(()(.()())).)(()(.(.",
                                    "(()(.()())).)(().))(.(())",
                                    "(()(.()())).)(().))(.(()).)((.("]
        ]

