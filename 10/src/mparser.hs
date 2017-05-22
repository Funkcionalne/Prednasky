import Control.Applicative 
import Control.Monad  (liftM, ap)
import Data.Char

--type Parser a = String -> [(a,String)]
data Parser a = Parser(String -> [(a,String)])
--newtype Parser a = Parser(String -> [(a,String)])

parse           :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

--infixr 6 <*>
--infixr 4 <|>

result      :: a -> Parser a
result v   = Parser(\xs -> [(v,xs)])
-- succeed      :: a -> Parser a
-- succeed v   = Parser(\xs -> [(v,xs)])

zero      :: Parser a
zero      = Parser(\xs -> [])
-- fail   :: Parser a
-- fail   = Parser(\xs -> [])

item      :: Parser Char
item      = Parser(\xs -> case xs of
            []   -> []
            (v:vs)   -> [(v,vs)])

seq      :: Parser a -> Parser b -> Parser (a,b)
(Parser p) `seq`   (Parser q)   = Parser(\xs -> [ ((v,w),xs'')| (v,xs') <- p xs, (w,xs'') <- q xs'])

-- seqq   :: Parser a -> Parser b -> Parser (a,b)
-- p `seqq`   q   = Parser(\xs -> [ ((v,w),xs'')| (v,xs') <- parse p xs, (w,xs'') <- parse q xs'])

-- (<*>)   :: Parser a -> Parser b -> Parser (a,b)
-- (Parser p) <*> (Parser q)   = Parser(\xs -> [ ((v,w),xs'')| (v,xs') <- p xs, (w,xs'') <- q xs'])

bind      :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f   =   Parser(\xs -> concat [ parse (f v) xs' | (v,xs')<-parse p xs])

-- seqUsingBin   :: Parser a -> Parser b -> Parser (a,b)
-- p `seqUsingBin` q = p `bind` \x->
--                 q `bind` \y->
--                 return (x,y)

sat            :: (Char->Bool) -> Parser Char
sat    pred   = item `bind` \x->
                if pred x then return x else zero

-- satisfy = sat

char    :: Char -> Parser Char
char x   = sat (\y -> x==y)

-- symbol = char

digit    :: Parser Char
digit   = sat isDigit

lower    :: Parser Char
lower   = sat isLower

upper    :: Parser Char
upper   = sat isUpper

plus   :: Parser a -> Parser a -> Parser a
p `plus` q = Parser(\xs -> (parse p xs ++ parse q xs))

-- (<|>) = plus

letter   :: Parser Char
letter   = lower `plus` upper

alphaNum :: Parser Char
alphaNum = letter `plus` digit

word   :: Parser String
word   = nonEmptyWord `plus` return "" where 
        nonEmptyWord = letter `bind` \x    ->
                     word   `bind` \xs       ->
                     return (x:xs)

instance Monad Parser where
  return v   = Parser(\xs -> [(v,xs)])
  p >>= f   = Parser(\xs -> concat [ parse (f v) xs' | (v,xs')<-parse p xs])

instance Functor Parser where
    fmap  = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap  
  
{-  
class Monad m => Monad0Plus m where
   mzero :: m a
   mplus :: m a -> m a -> m a

instance Monad0Plus Parser where
--  mzero      = Parser(\xs -> [])
  mzero      = zero
--  mplus p  q   = Parser(\xs -> (parse p xs ++ parse q xs))
  mplus p  q   = p `plus` q
-}

string      :: String -> Parser String
string   ""   = return ""
string (x:xs)   = char x >>= \_ ->
                string xs >>= \_ ->
                return (x:xs)
                
string'          :: String -> Parser String
string' ""        = return ""
string' (c:cs)    = do {char c; string' cs; return (c:cs)}

sat'   :: (Char->Bool) -> Parser Char
sat'    pred   = do { x<-item; if pred x then return x else zero}

many1           :: Parser a -> Parser [a]
many1 p          = do {a <- p; as <- many' p; return (a:as)} 

many'            :: Parser a -> Parser [a]
many' p           = many1 p +++ return []

sepby         :: Parser a -> Parser b -> Parser [a]
p `sepby` sep   = (p `sepby1` sep) +++ return []

sepby1         :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do { a<-p; as<- many'(do {sep;p}); return (a:as) }

--------------------

(+++)           :: Parser a -> Parser a -> Parser a
p +++ q         = Parser (\cs -> case parse (p `plus` q) cs of
                                     []     -> []
                                     (x:xs) -> [x]) 
                                     
word'   :: Parser String
word'   = nonEmptyWord +++ return ""
     where nonEmptyWord = letter `bind` \x    ->
                       word'   `bind` \xs    ->
                        return (x:xs)
                                     
identifier      :: Parser String
identifier       = do {c <- lower; cs <- many' alphaNum; return (c:cs)}
                                
space           :: Parser String
space            = many' (sat isSpace)

token           :: Parser a -> Parser a
token p          = do {a <- p; space; return a}

apply           :: Parser a -> String -> [(a,String)]
apply p          = parse (do {space; p})

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do {a <- p; rest a}
                   where
                      rest a = (do {f <- op; b <- p; rest (f a b)}) 
                               `plus`
                               return a
                               
digit'           :: Parser Int
digit'            = do {c <- sat isDigit; return (ord c - ord '0')}
                                     
natural         :: Parser Int
natural          = digit' `chainl1` (return (\m n -> 10*m + n))

nat             :: Parser Int
nat              = token natural

int             :: Parser Int
int              = token integer

integer         :: Parser Int
integer          = do {char '-'; n <- natural; return (-n)} +++ nat
-----------------

open      :: Parser Char
open      = char '('
close      = char ')'

paren      :: Parser ()
paren      =   do { open; paren; close; paren; return () } +++ return ()

data Bin   = Nil | Node Bin Bin deriving(Show, Read, Eq) 
      
parenBin   :: Parser Bin
parenBin   =   do { open; x<-parenBin; close; y<-parenBin; return (Node x y) } +++ return Nil

openBr      = char '['
closeBr      = char ']'

-- P -> (P)P | [P]P | eps

-- Main> parse parenBr "([[(])])([])"
-- [((),"")]
-- zle :-)
-- parenBr      :: Parser ()
-- parenBr      =   do { (open `plus` openBr) ; parenBr; (close `plus` closeBr) ; parenBr; return () } +++ return ()

parenBr      :: Parser ()
parenBr      =   do { open; parenBr; close; parenBr; return () }
            +++
            do { openBr ; parenBr; closeBr ; parenBr; return () } 
            +++ 
            return ()

-----------------
symb            :: String -> Parser String
symb cs          = token (string cs)


expr  :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

{- najivna verzia, zacykli sa
expr   = do {x <- expr; f <- addop; y<-factor; return (f x y)} `plus` factor
factor   = nat `plus` do { open; x<-expr; close; return x}
-}

addop   = do { char '+'; return (+) } `plus` do { char '-'; return (-) } 
mulop   = do { char '*'; return (*) } `plus` do { char '/'; return (div) } 


expr   = term   `chainl1` addop
term   = factor `chainl1` mulop
--factor = mplus (token digit') (do {symb "("; n <- expr; symb ")"; return n})
-- factor = mplus (token nat) (do {symb "("; n <- expr; symb ")"; return n})
factor = nat `plus` (do {open; n <- expr; close; return n})


       