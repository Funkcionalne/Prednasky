import Data.Char
import Control.Monad
import Control.Monad.State

data Term = Con Int | Div Term Term deriving(Show, Read, Eq)
type Stav = Int

evalSM           :: Term -> State Stav Int
evalSM (Con a)   = return a
evalSM (Div t u) = do valT<-evalSM t
                      valU<-evalSM u
                      modify (+1)
                      return(valT `div` valU)

t :: Term
t = (Div (Div (Con 1972) (Con 2)) (Con 23))



