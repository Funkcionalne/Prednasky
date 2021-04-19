import System.IO
import System.Environment
import Data.Char

lopaty :: String -> Bool
lopaty _ = False

main :: IO ()        
main = do args <- getArgs
          let n = read (args!!0)
          putStrLn (show $ lopaty n)
