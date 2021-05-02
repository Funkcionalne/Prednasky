import Control.Monad.State

-- https://wiki.haskell.org/State_Monad
{-
runState :: State s a -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s
-}

e1=runState ((return "hello") :: State Int String) 1 == ("hello",1)

e2=(runState ((return "hello") :: State Int String)) :: Int -> (String, Int)

e3=runState get 1 == (1,1)
e4=runState get "Hello" == ("Hello","Hello")

e5=runState (put 5) 1

e6=runState (do { put 5; return 'X' }) 1 == ('X',5)

e7=let postincrement = do { x <- get; put (x+1); return x } in runState postincrement 1

e9=runState (modify (+1)) 1

e10=evalState (gets (+1)) 1  == 2
e11=execState (gets (+1)) 1  == 1



{-
return :: a -> State s a
return x s == (x,s)

get :: State s s
get s == (s,s)

put :: s -> State s ()
put x s = ((),x)

modify :: (s -> s) -> State s ()
modify f = do { x <- get; put (f x) }

gets :: (s -> a) -> State s a
gets f = do { x <- get; return (f x) }

e22=evalState :: State s a -> s -> a
e23=evalState act == fst . runState act

e24=execState :: State s a -> s -> s
e25=execState act == snd . runState act

-}




