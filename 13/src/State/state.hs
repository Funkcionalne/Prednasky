import Control.Monad.State.Lazy

-- https://wiki.haskell.org/State_Monad
{-
runState :: State s a -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s
-}

runState ((return "hello") :: State Int String) 1 = ("hello",1)

(runState ((return "hello") :: State Int String)) :: Int -> (String, Int)

runState get 1 = (1,1)
runState get "Hello" = ("Hello","Hello")

runState (put 5) 1

runState (do { put 5; return 'X' }) 1 = ('X',5)

let postincrement = do { x <- get; put (x+1); return x } in runState postincrement 1

runState gets (+1) 1


runState (modify (+1)) 1

evalState (gets (+1)) 1  = 2
execState (gets (+1)) 1  = 1

return :: a -> State s a
return x s = (x,s)

get :: State s s
get s = (s,s)

put :: s -> State s ()
put x s = ((),x)

modify :: (s -> s) -> State s ()
modify f = do { x <- get; put (f x) }

gets :: (s -> a) -> State s a
gets f = do { x <- get; return (f x) }

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

