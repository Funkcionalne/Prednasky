module Users where

import Control.Monad.IO.Class
import Data.Char

main :: IO ()
main = do login <- readLogin
          case login of
            Nothing -> print "Invalid user"
            Just (user) -> do password <- readPassword
                              case password of
                                 Nothing -> print "Invalid password"
                                 Just password -> putStrLn $ "logged: " ++ user

readLogin :: IO (Maybe String)
readLogin = do putStrLn "login:"
               str <- getLine
               if length str > 5
                 then return $ Just str
                 else return Nothing

readPassword :: IO (Maybe String)
readPassword = do putStrLn "password:"
                  str <- getLine
                  if length str < 8 || null (filter isUpper str) || null (filter isLower str)
                    then return Nothing
                    else return $ Just str
