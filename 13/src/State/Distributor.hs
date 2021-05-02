module Distributor where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
  
-- Mlsny automat
type Produkt = (Int, Float)   -- pocet, cena

type Vysledok = Int
data Stav = Stav { produkty :: M.Map String Produkt, mince :: M.Map Float Int } deriving (Show)
initState :: Stav
initState = 
        Stav {
            produkty = M.fromList [("sojovka", (10,0.4)),("horalka", (20,0.5)), ("mila", (10,0.5)), ("twix", (4,0.8)), ("3bit", (3, 1.0))],
            mince = M.fromList [(0.05, 4),(0.1, 14),(0.2, 44),(0.5, 24),(1.0, 10),(2.0, 16)]
        }

putCoin :: Float -> State Stav ()
putCoin coin = do stav <- get
                  let coins = mince stav
                  let pocet = M.lookup coin coins
                  let newCoins = if isJust pocet then -- nasla sa
                                    M.insert coin (1+(fromJust pocet)) coins
                                 else M.insert coin 1 coins
                  put $ Stav (produkty stav) newCoins

takeProduct :: String -> State Stav () -- Bool
takeProduct pname = do stav <- get
                       let products = produkty stav
                       let product = M.lookup pname products
                       let newProducts = if isJust product then -- nasiel sa
                                             M.insert pname 
                                                      (let (pocet, cena) = fromJust product in (if pocet > 0 then pocet-1 else 0, cena))
                                                      products
                                         else
                             products
                       put $ Stav newProducts (mince stav)
                       --return $ products == newProducts

main :: IO ()
main = putStrLn $ show $ execState (sequence[putCoin 1.0, takeProduct "horalka"]) initState
