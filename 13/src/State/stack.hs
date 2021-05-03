module Stack where
import Control.Monad.State  

type Stack = [Int]  
  
{-  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
-}

--     newtype State s a = State { runState :: s -> (a,s) }  
{-
    instance Monad (State s) where  
        return x = State $ \s -> (x,s)  
        (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                            (State g) = f a  
                                        in  g newState  
-}

  
pop :: State Stack Int  
pop = state(\(x:xs) -> (x,xs))  
  
push :: Int -> State Stack ()  
push a = state(\xs -> ((),a:xs))

pushAll :: Int -> State Stack String
pushAll 0   = return ""
pushAll n   = do
                push n
                str <- pushAll (n-1)
                nn <- pop
                return (show nn ++ str)
                
{-
"?: " evalState (pushAll 10) []
"10987654321"
"?: " execState (pushAll 10) []
[]
-}                

pushAll' :: Int -> State Stack String
pushAll' 0   = return ""
pushAll' n   = do
                    stack <- get  -- push n
                    put (n:stack)
                    str <- pushAll' (n-1)
                    xx <- get  
                    let (nn:stack') = xx
                    put stack'
                    return (show nn ++ str)






























































fooo :: Int -> State Stack String
fooo 0   = return ""
fooo n   = do  modify (n:)
                    str <- fooo (n-1)
                    xx <- get  
                    modify tail
                    return (show (head xx) ++ str)
                