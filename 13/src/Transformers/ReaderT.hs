module Transformers where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Char

type Env = (Maybe String, Maybe String)

{-
login :: MaybeT (ReaderT Env IO) String
login = MaybeT $ do
  (oldLogin, _) <- ask
  case oldLogin of
    Just str -> return $ Just str
    Nothing -> do
      -- lift allows normal IO functions from inside ReaderT Env IO!
      lift $ putStrLn "login:"
      input <- lift getLine
      if length input > 5
        then return (Just input)
        else return Nothing
-}

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
        
main :: IO ()
main = do loginPassword <- runMaybeT $ do
              login <- readLogin
              liftIO $ putStrLn ("login: " ++ login)
              password <- readPassword
              liftIO $ putStrLn ("pass: " ++ password)
              return (login, password)
          case loginPassword of
            Nothing -> print "Invalid user or password"
            Just (user, _) -> putStrLn $ "logged: " ++ user        