import Control.Applicative
main :: IO ()
main = do
          ---res <- words <$> getLine
          res <- fmap words getLine
          putStrLn $ show res