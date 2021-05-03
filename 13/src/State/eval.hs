import Data.Char
import Control.Monad
--import Control.Monad.State
import Data.Functor
import Control.Applicative

data Term = Con Int | Div Term Term deriving(Show, Read, Eq)

type State = Int
data SM a       =  SM (State-> (a, State)) 

instance Functor SM where
       fmap f (SM g) = (SM ( \x -> let (a,x') = g x in (f a, x')   ))
       
instance Applicative SM where
       pure       = return
       mf <*> ma = do { f <- mf; a <- ma; return (f a) }

instance Monad SM where
       return v       = SM(\x -> (v, x))
       (SM p) >>= f   = SM(\x -> let (a,y) = p x in
                                 let SM g = f a in 
                                 g y)

incState       :: SM ()
incState       = SM (\x -> ((),x+1))

evalSM          :: Term -> SM Int
evalSM(Con a)   = return a
evalSM(Div t u) = evalSM t >>= \valT-> evalSM u >>= \valU -> incState >>= \_ -> return(valT `div` valU)


goSM       :: Term -> State
goSM t        = let SM p = evalSM t in 
                let (result,state) = p 0 in state
------------------------------------------------------------------------
evalSM'          :: Term -> SM Int
evalSM'(Con a)   = return a
evalSM'(Div t u) = do valT<-evalSM' t
                      valU<-evalSM' u
                      incState
                      return(valT `div` valU)
                                            
goSM'              :: Term -> State
goSM' t        = let SM p = evalSM' t in 
                  let (result,state) = p 0 in state

t :: Term
t = (Div (Div (Con 1972) (Con 2)) (Con 23))

-- Main> goSM (Div (Div (Con 1972) (Con 2)) (Con 23))
-- 2

