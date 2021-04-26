import Control.Monad 
import System.Directory
import System.Environment

-- System.Dirctory
-- doesDirectoryExist has type FilePath -> IO Bool

main :: IO ()
main = do dir <- getArgs
          filesOrDirs <- mapM listDirectory dir
          putStrLn $ show filesOrDirs
