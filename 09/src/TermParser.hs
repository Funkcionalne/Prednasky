module TermParser where
import Data.Char

data Term = Var String | 
            CN Int | 
            Functor String [Term]   -- deriving(Show)
instance Show Term where
    show (CN n) = show n
    show (Var name) = name
    show (Functor f args) = f ++ if null args then [] 
                                 else 
                                   "(" ++ (show (args!!0))++ (concat [ "," ++ (show (args!!i))  | i <- [1..length args-1] ]) ++ ")" 

fromStringArgs  :: String -> ([Term], String)
fromStringArgs xs = let (a, xs') = fromString xs in 
                       if not (null xs') &&  head xs' == ',' then let (as,xs'') = fromStringArgs (tail xs') in ((a:as), xs'')
                       else ([a], tail xs')


fromString  :: String -> (Term, String)
fromString (x:xs)   | isDigit x  = (CN (ord x-48), xs)
fromString [x]      | isAlpha x && isLower x  = (Functor [x] [], [])
fromString (x:y:xs) | isAlpha x && isLower x && y == '(' = let (args, xs') = fromStringArgs xs in (Functor [x] args, xs')
fromString (x:xs)   | isAlpha x && isUpper x  = (Var [x], xs)
fromString  xs      = error ("syntax error: " ++ xs)                                                   
