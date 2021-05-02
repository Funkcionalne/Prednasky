import Control.Monad
import Control.Monad.Writer

calc :: Integer -> Writer String Integer
calc n = do line "starting"
            let f = product [1..n]
            line $ (show n) ++ " != " ++ take 10 (show f)
            line "finishing"
            return f

line :: String -> Writer String ()
line x = tell x

main = mapM_ print $ 
        map (execWriter . calc) [10,100,1000,10000,100000]