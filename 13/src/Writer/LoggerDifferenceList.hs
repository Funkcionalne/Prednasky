-- http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/
-- https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf
import Control.Monad
import Control.Monad.Writer
import Data.DList

calc :: Integer -> Writer (Dlist String) Integer
calc n = do line "starting"
            let f = product [1..n]
            line $ (show n) ++ " != " ++ take 10 (show f)
            line "finishing"
            return f

line :: String -> Writer (Dlist String) ()
line x = tell (singleton x)

main = mapM_ print $ 
        map (execWriter . calc) [10,100,1000,10000,100000]