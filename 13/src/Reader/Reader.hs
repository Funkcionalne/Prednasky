module Reader where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main :: IO ()
main = do params <- loadParams
          let result = func1 params
          putStrLn result
          
data Params = Params { p1 :: String, p2 :: String, p3 :: String }     deriving (Show)

loadParams :: IO Params
loadParams = do 
                p1 <- lookupEnv "JAVA_HOME"
                p2 <- lookupEnv "OS"
                p3 <- lookupEnv "HOMEDRIVE"
                return $ Params
                        (fromMaybe "no java" p1)
                        (fromMaybe "unknown" p2)
                        (fromMaybe "no drive" p3)
----------------------------
func1 :: Params -> String
func1 params = "Result: " ++ (show (func2 params))
--func1 params = "Result: " ++ (show params)

func2 :: Params -> Int
func2 params = 2 + floor (func3 params)

func3 :: Params -> Float
func3 params = (fromIntegral $ length $ p1 params ++ p2 params ++ p3 params) * 3.14

-------
func1' :: Reader Params String
func1' = do params <- ask
            result <- func2'
            return $ "Result: " ++ (show result)

func2' :: Reader Params Int
func2' = do params <- ask
            result <- func3'
            return $ 2+floor(result)
           
func3' :: Reader Params Float
func3'  = do params <- ask
             let result = (fromIntegral $ length $ p1 params ++ p2 params ++ p3 params) * 3.14
             return result
             
main' :: IO ()
main' = do params <- loadParams
           let result = runReader func1' params
           putStrLn result             
           let result1 = runReader (local (\p -> Params { p1 = "aa", p2 = "bb", p3 ="cc" }) func1') params
           putStrLn result1

