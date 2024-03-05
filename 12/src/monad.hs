import Data.Char
import Control.Monad
--import Control.Monad.State
import Data.Functor
import Control.Applicative

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

a každá inštancia musí spaoa?

fmap id      = id
fmap (p . q) = (fmap p) . (fmap q)
-}

--data M1 a =       Raise String | Return a
--              deriving(Show, Read, Eq)

instance Functor M1  where
       fmap f (Raise str)    =  Raise str
       fmap f (Return x)     =  Return (f x)

data MyMaybe a = MyJust a | MyNothing deriving (Show)  -- Maybe a

instance Functor MyMaybe  where
       fmap f MyNothing    =  MyNothing
       fmap f (MyJust x)   =  MyJust (f x)

{-
instance Functor Maybe  where
       fmap f Nothing    =  Nothing
       fmap f (Just x)   =  Just (f x)
-}

{-
instance  Functor []  where
       fmap = map
-}

data MyList a = Null | Cons a (MyList a) deriving (Show)       -- [a]

instance  Functor MyList  where
       fmap f Null = Null
       fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a) deriving (Show)
instance Functor BinTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)


data LExp a = Var a | Appl (LExp a) (LExp a) | Abs a (LExp a) deriving (Show)
instance Functor LExp where
       fmap f (Var x)                             = Var (f x)
       fmap f (Appl left right)        = Appl (fmap f left) (fmap f right)
       fmap f (Abs x right)               = Abs (f x) (fmap f right)       

omega = Abs "x" (Appl (Var "x") (Var "x"))

data RoseTree a = Rose a [RoseTree a]
instance Functor RoseTree where
       fmap f (Rose a bs)       = Rose (f a) (map (fmap f) bs)

roseTree = Rose "a" [Rose "b" [],Rose "c" [],Rose "d" []]       
-------------------------------------

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- (*>) :: f a -> f b -> f b
-- (<*) :: f a -> f b -> f a

--data M1 a =       Raise String | Return a
--              deriving(Show, Read, Eq)


instance Applicative M1 where
       pure a       =       Return a
       Raise e <*>       _       = Raise e
       (Return f) <*> a = fmap f a

{-       
> Return (+4) <*> Return 10
Return 14
> pure (+4) <*> Return 10
Return 14
> pure (\x -> x++x) <*> Return "aha"
Return "ahaaha"
-}

{-
instance Applicative [] where
       pure a              =       [a]
       fs <*> xs       = [ f x | f <- fs, x<-xs]
-}
-- [(+1),(*2),(^3)] <*> [10,20,30]
-------------------------------------
data Term = Con Int | Div Term Term deriving(Show, Read, Eq)

eval          :: Term -> Int
eval(Con a)   = a
eval(Div t u) = eval t `div` eval u

-- Main> eval (Div (Div (Con 1972) (Con 2)) (Con 23))
-- 42

--------------------------------------

data M1 a         = Raise String | Return a deriving(Show, Read, Eq)

evalExc               :: Term -> M1 Int
evalExc(Con a)        = Return a
evalExc(Div t u)      = case evalExc t of
                        Raise e -> Raise e
                        Return a ->
                          case evalExc u of
                            Raise e -> Raise e
                            Return b ->
                                if b == 0
                                  then Raise "div by zero"
                                  else Return (a `div` b)

-- Main> evalExc (Div(Con 1)(Con 0))
-- Raise "div by zero"

----------------------------------------

type M2 a         = State -> (a, State)
type State       = Int

evalCnt             :: Term -> M2 Int
evalCnt (Con a) x   = (a,x)
evalCnt (Div t u) x = let (a,y) = evalCnt t x in
                       let (b,z) = evalCnt u y in
                         (a `div` b,z+1)

-- Main> evalCnt (Div (Div (Con 1972) (Con 2)) (Con 23)) 0
-- (42,2)

-----------------------------------------

type M3 a         = (Output, a)
type Output      = String

evalOut               :: Term -> M3 Int
evalOut(Con a)        = (line(Con a) a, a)
evalOut(Div t u)      = let (x,a) = evalOut t in
                         let (y,b) = evalOut u in
                           (x ++ y ++ line(Div t u)(a `div` b), a `div` b)

line        :: Term -> Int -> Output
line t a    = "eval (" ++ show t ++ ") <=" ++ show a ++ "\n"

-- Main> evalOut (Div (Div (Con 1972) (Con 2)) (Con 23))
-- ("eval (Con 1972) <=1972\neval (Con 2) <=2\neval (Div (Con 1972) (Con 2)) <=986\neval (Con 23) <=23\neval (Div (Div (Con 1972) (Con 2)) (Con 23)) <=42\n",42)

----------------------------------------------
-- return  :: a -> M a
-- >>=     :: M a -> (a -> M b) -> M b
----------------------------------------------
newtype Identity a = Identity a deriving(Show, Read, Eq)

instance Functor Identity where
       -- fmap :: (a -> b) -> f a -> f b
       fmap f (Identity a)    =  Identity (f a)

instance Applicative Identity where
       pure       = return
       mf <*> ma = do { f <- mf; a <- ma; return (f a) }
       
instance Monad Identity where
  return v                     = Identity v
  (Identity p) >>= f       =  f p 


evalIdentM          :: Term -> Identity Int
evalIdentM(Con a)   = return a
evalIdentM(Div t u) = evalIdentM t >>= \valT-> evalIdentM u >>= \valU -> return(valT `div` valU)

evalIdentM'          :: Term -> Identity Int
evalIdentM' (Con a)   = return a
evalIdentM' (Div t u) = do valT<-evalIdentM' t; valU <-evalIdentM' u; return(valT `div` valU)


-- Main> evalIdentM (Div (Div (Con 1972) (Con 2)) (Con 23))
-- Identity 42

-----------------------------------------------

data Exception a = Raise' String | Return' a 
                     deriving(Show, Read, Eq)
                     
instance Functor Exception where
       fmap f (Raise' str)    =  Raise' str
       fmap f (Return' a)    =  Return' (f a)
       
instance Applicative Exception where
       -- pure :: a -> f a
       pure       = return
       -- (<*>) :: f (a -> b) -> f a -> f b
       mf <*> ma = do { f <- mf; a <- ma; return (f a) }
                     
instance Monad Exception where
       return v       = Return' v
       p >>= f       =  case p of
                             Raise' e -> Raise' e
                             Return' a -> f a
                       
raise       :: String -> Exception a
raise e       = Raise' e

evalExceptM          :: Term -> Exception Int
evalExceptM(Con a)   = return a
evalExceptM(Div t u) = evalExceptM t >>= \valT-> 
                       evalExceptM u >>= \valU -> 
                       if valU == 0 then raise "div by zero" else return(valT `div` valU)

evalExceptM'         :: Term -> Exception Int
evalExceptM'(Con a)   = return a
evalExceptM'(Div t u) = do{ 
                             valT<-evalExceptM' t;
                             valU<-evalExceptM' u;
                             if valU == 0 then raise "div by zero" else return(valT `div` valU)
                          }

{-
evalExceptM'(Div t u) = do valT<-evalExceptM' t
                           valU<-evalExceptM' u
                           if valU == 0 then raise "div by zero" else return(valT `div` valU)
-}

-- Main>  evalExceptM (Div (Div (Con 1972) (Con 2)) (Con 23))
-- Return' 42

---------------------------------------------------

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
--incState       :: SM Bool
--incState       = SM (\x -> (True,x+1))

evalSM          :: Term -> SM Int
evalSM(Con a)   = return a
evalSM(Div t u) = evalSM t >>= \valT-> evalSM u >>= \valU -> incState >>= \_ -> return(valT `div` valU)


goSM       :: Term -> State
goSM t        = let SM p = evalSM t in 
                let (result,state) = p 0 in state

evalSM'          :: Term -> SM Int
evalSM'(Con a)   = return a
evalSM'(Div t u) = do valT<-evalSM' t
                      valU<-evalSM' u;
                      incState
                      return(valT `div` valU)
                                            
goSM'              :: Term -> State
goSM' t        = let SM p = evalSM' t in 
                  let (result,state) = p 0 in state

-- Main> goSM (Div (Div (Con 1972) (Con 2)) (Con 23))
-- 2

---------------------------------------------------

data Out a       =  Out(String, a) deriving(Show, Read, Eq)

instance Functor Out where
       fmap f (Out (str, a))    =  (Out (str, f a))
       
instance Applicative Out where
       pure       = return
       mf <*> ma = do { f <- mf; a <- ma; return (f a) }
                     
instance Monad Out where
  return v       = Out("",v)
  p >>= f         = let Out (str1,y) = p in
                  let Out (str2,z) = f y in 
                         Out (str1++str2,z)

out       ::        String -> Out ()
out s       =       Out (s,())

evalOutM          :: Term -> Out Int
evalOutM(Con a)   = do           out(line(Con a) a);
                                                                                                                        return a
evalOutM(Div t u) = do valT<-evalOutM t;
                                                                                                               valU<-evalOutM u;
                       out (line (Div t u) (valT `div` valU) );
                       return (valT `div` valU)

-- Main> evalOutM (Div (Div (Con 1972) (Con 2)) (Con 23))
-- Out ("eval (Con 1972) <=1972\neval (Con 2) <=2\neval (Div (Con 1972) (Con 2)) <=986\neval (Con 23) <=23\neval (Div (Div (Con 1972) (Con 2)) (Con 23)) <=42\n",42)

--------------------------------------------------

data TTerm       = TVar String | TTerm :->: TTerm | CN String
                     deriving(Show, Read, Eq)

test        :: Bool -> a -> Maybe a
test       cond v        | cond       = Just v
                            | otherwise       = Nothing
                            
type Subst = [(String, TTerm)]

occur        :: String -> TTerm -> Bool
occur _ _ = False       -- toto treba dorobit

value :: String->Subst ->TTerm
value x []                             = TVar x
value x ((y,t):s)       | x==y       = t
                                   | otherwise = value x s

unify       :: TTerm -> TTerm -> Subst -> Maybe Subst
unify       (TVar x) y s       | (TVar x)/=t         = unify t y s where t = value x s
unify       x (TVar y) s       | (TVar y)/=t       = unify x t s where t = value y s
unify        (CN x) (CN y) s        = test (x == y) s
unify       (TVar x) (TVar y) s       | x == y       = return s
                                                 | x < y              = return ((x,TVar y):s)
                                                 | y < x              = return ((y,TVar x):s)
unify       (TVar x) t s       = test (not (occur x t)) ((x,t):s)
unify       t (TVar x) s       = test (not (occur x t)) ((x,t):s)

unify       (t1 :->: t2) (u1 :->: u2) s       = do { s1 <- unify t1 u1 s; s2 <- unify t2 u2 s1; return s2 }
                                   
t1 = (TVar "x") :->: ((TVar "u") :->: (TVar "x")) :->: (TVar "w")
t2 = (TVar "y") :->: ((TVar "x") :->: (CN "Int")):->: (CN "Real")

-------------------------------------------


-- data IO a = ... {- abstract -}

-- getChar ::         IO Char
-- putChar :: Char -> IO ()
-- getLine :: IO String
-- putStr :: String -> IO ()
-- (>>) :: IO a -> IO b -> IO b
-- (>>=) :: IO a -> (a -> IO b) -> IO b

echo = getChar >>= putChar

main1 = putStr "Please enter your name: " >>
           getLine                           >>= \name ->
           putStr ("Hello, " ++ name ++ "\n")

main2 = do
    putStr "Please enter your name: "
    name <- getLine
    putStr ("Hello, " ++ name ++ "\n")

--------------------------------------------------------

listComprehension xs ys = [(x,y) | x<-xs, y<-ys ]
guardedListComprehension xs ys = [(x,y) | x<-xs, y<-ys, x<=y, x*y == 24 ]


monadComprehension xs ys = do { x<-xs; y<-ys; return (x,y) }
guardedMonadComprehension xs ys = do { x<-xs; y<-ys; guard (x<=y); guard (x*y==24); return (x,y) }

{-
listComprehension [1,2,3] ['a','b','c']
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

guardedListComprehension [1..10] [1..10]
[(3,8),(4,6)]


monadComprehension [1,2,3] ['a','b','c']
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

ain> guardedMonadComprehension [1..10] [1..10]
[(3,8),(4,6)]

-}

guardedComprehension :: [Int] -> [Int] -> [(Int,Int)]
guardedComprehension xs ys = do { x<-xs; y<-ys; guard (x*y == 8); return (x,y) }

{-
guardedComprehension [1..10] [1..10]
[(1,8),(2,4),(4,2),(8,1)]
-}


sequenceUsingFold :: Monad m => [m a] -> m [a]
sequenceUsingFold = foldr (\c cs -> do { x <- c; xs <- cs; return (x:xs) }) (return [])

sequence2       :: Monad m => [m a] -> m [a] 
sequence2 ms = foldr k (return []) ms
            where
              k m m' = do { x <- m; xs <- m'; return (x:xs) }                       