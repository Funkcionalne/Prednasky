module Users where

import Control.Monad.IO.Class
import Data.Char
import Control.Monad.Trans.Maybe

main :: IO ()
main = do loginPassword <- runMaybeT $ do
              login <- readLogin
              password <- readPassword
              return (login, password)
          case loginPassword of
            Nothing -> print "Invalid user or password"
            Just (user, _) -> putStrLn $ "logged: " ++ user

readLogin :: MaybeT IO String
readLogin = MaybeT $ 
            do putStrLn "login:"
               str <- getLine
               if length str > 5
                 then return $ Just str
                 else return Nothing

readPassword :: MaybeT IO String
readPassword = MaybeT $ 
               do putStrLn "password:"
                  str <- getLine
                  if length str < 8 || null (filter isUpper str) || null (filter isLower str)
                    then return Nothing
                    else return $ Just str
