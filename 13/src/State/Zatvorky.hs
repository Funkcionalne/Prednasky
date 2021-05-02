module Zatvorky where
import Control.Monad.State

type Value = Int
type Stav = (Bool, Int, Int)

loop :: String -> State Stav Bool
loop []     = do (ok, parents, brackets) <- get
                 return $ ok && parents == 0 && brackets == 0
loop (x:xs) = do
    (ok,parents, brackets) <- get
    case x of
         '('      -> put (ok,parents+1, brackets)
         '['      -> put (ok,parents, brackets+1)
         ')' | parents > 0    -> put (ok,parents-1, brackets) 
         ')' | parents <= 0 -> put (False,0,0)
         ']'      -> put $ if (brackets>0) then (ok,parents, brackets-1) else (False,0,0)
    loop xs

main :: IO ()
main = mapM_ print [ input ++ " -> "++ show (evalState (loop input) (True, 0, 0)) | input <- [
                                    "()", "(())", "(()())", "(()()(()))",
                                    ")(", "([)]", "[[]]", "][", "()[]"]
        ]
