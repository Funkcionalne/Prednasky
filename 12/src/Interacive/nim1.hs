-- jednokopovy nim
-- nim2 hraju dvaja hraci, zacina True, pokracuje False
-- z kopy striedavo beru 1,2 alebo 3 zapalky, kto berie poslednu, vyhrava

import Data.Char
import System.IO.Unsafe 
import System.Random
 
-- nahodne cislo 0..n-1
nextInt :: Int -> Int -> Int
nextInt a b = fromIntegral $ unsafePerformIO $ randomRIO (a, b-1) 

type Kopa = Int

-- kedy hra konci
finished :: Kopa -> Bool
finished = (== 0)

-- korektny tah
valid :: Kopa -> Int -> Bool
valid kopa num = (kopa >= num) && num < 4

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     if isDigit x 
                        then return (digitToInt x)
                     else
                        getDigit ""

-- dvaja hraci
play2 :: Kopa -> Bool -> IO ()
play2 kopa hrac =
   do putStrLn ("kopa:" ++ (show kopa))
      if finished kopa then
         putStrLn ("Hrac " ++ (show (not $ hrac)) ++ " vyhral!")
      else  
         do putStrLn ("Ide hrac " ++ (show hrac))
            num <- getDigit "kolko beries : "
            if valid kopa num then
               play2 (kopa - num) (not $ hrac)
            else
               do putStrLn "zly tah"
                  play2 kopa hrac

-- dvaja hraci
nim2 :: IO ()
nim2 = play2 (nextInt 10 20) True

-- jeden hraci
strategia :: Int -> Int
strategia kopa | kopa `mod` 4 == 0 = kopa-1
               | otherwise         = kopa - (kopa `mod` 4)

play1 :: Kopa -> IO ()
play1 kopa =
   do putStrLn ("kopa:" ++ (show kopa))
      if finished kopa then
         putStrLn "prehral si :("
      else  
         do num <- getDigit "kolko beries : "
            if valid kopa num then
               let kopa' = kopa - num in
                   if finished kopa' then
                      putStrLn "vyhral si :)"
                   else
                      do putStrLn ("ja beriem:" ++ (show (kopa' - (strategia kopa'))))
                         play1 (strategia kopa')
            else
               do putStrLn "zly tah"
                  play1 kopa 

-- jeden hrac proti kompu
nim1 :: IO ()
nim1 = play1 (nextInt 10 20)
