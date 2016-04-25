module Parser 
    ((<*>), (<*), (*>), (<:*>), (<@), (<?@), (<|>),
    failp, succeed, epsilon,
    satisfy, symbol, token,
    option, many, many1,
    determ, compulsion, greedy, greedy1,
    pack, listOf, chainl, chainr, Parser.sequence, choice,
    just, some,
    Parser
    )
where

import Prelude hiding ((<*>), (*>), (<*))
import Data.Char

type Parser symbol result  =  [symbol] -> [([symbol],result)]

-- A parser that yields just one solution and no rest-string
-- is called a deterministic parser:

type DetPars symbol result  =  [symbol] -> result

-- Priorities of operators

infixr 6 <*> 
infixr 6 <*
infixr 6 *> 
infixr 6 <:*>
infixl 5 <@
infixl 5 <?@
infixr 4 <|>

-- Auxiliary functions

list (x,xs)  =  x:xs
single x     =  [x]

ap2 (op,y)   = (`op` y)
ap1 (x,op)   = (x `op`)

ap  (f,x)    = f x
ap' (x,f)    = f x

-- Trivial parsers

failp    ::  Parser s r
failp xs  =  []

succeed       ::  r -> Parser s r
succeed v xs   =  [ (xs,v) ]

epsilon  ::  Parser s ()
epsilon   =  succeed ()

-- Elementary parsers

satisfy  ::  (s->Bool) -> Parser s s
satisfy p []     =  []
satisfy p (x:xs) =  [ (xs,x) | p x ]

symbol  ::  Eq s  =>  s -> Parser s s
symbol a []      =  []
symbol a (x:xs)  =  [ (xs,x) | a==x ]

symbol'' a []                    =  []
symbol'' a (x:xs)  |  a==x       =  [ (xs,x) ]
                   |  otherwise  =  []

--token  ::  Eq [s]  =>  [s] -> Parser s [s]
--token   =  sequence . map symbol 

token k xs  |  k==take n xs  =  [ (drop n xs, k) ]
            |  otherwise     =  []
               where  n = length k    
                              
-- Parser combinators 

(<*>)          ::  Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2) xs  =  [  (xs2,(v1,v2)) 
                   |  (xs1, v1) <- p1 xs
                   ,  (xs2, v2) <- p2 xs1
                   ]

(<|>)          ::  Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs  =  p1 xs ++ p2 xs

(<@)          ::  Parser s a -> (a->b) -> Parser s b
(p <@ f) xs   =   [ (ys, f v)
                  | (ys,   v) <- p xs
                  ]

option   ::  Parser s a -> Parser s [a]
option p  =  p  <@  single
             <|> succeed []

many     ::  Parser s a  -> Parser s [a]
many p    =  p <:*> (many p)  <|>  succeed []

many1    ::  Parser s a -> Parser s [a]
many1 p   =  p <:*> many p 
               
-- Determinsm


determ :: Parser a b -> Parser a b
determ p xs  |  null r     =  []
             |  otherwise  =  [head r]
                     where r = p xs

compulsion = determ . option

greedy = determ . many

greedy1 = determ . many1
 
-- Abbreviations

(<*)         ::  Parser s a -> Parser s b -> Parser s a
p <* q        =  p <*> q  <@  fst

(*>)         ::  Parser s a -> Parser s b -> Parser s b
p *> q        =  p <*> q  <@  snd

(<:*>)       ::  Parser s a -> Parser s [a] -> Parser s [a]
p <:*> q      =  p <*> q  <@  list

(<?@)        ::  Parser s [a] -> (b,a->b) -> Parser s b
p <?@ (n,y)   =  p <@ f
          where  f []  = n
                 f [h] = y h               
 
------------
pack         ::  Parser s a -> Parser s b -> Parser s c -> Parser s b
pack s1 p s2  =  s1 *> p <* s2

listOf       ::  Parser s a -> Parser s b -> Parser s [a]
listOf p s    =  p <:*> many (s *> p)  <|>  succeed []

chainl       ::  Parser s a -> Parser s (a->a->a) -> Parser s a
chainl p s    =  p <*> many (s <*> p)  <@  uncurry (foldl (flip ap2))


chainr       ::  Parser s a -> Parser s (a->a->a) -> Parser s a
chainr p s    =  many (p <*> s) <*> p  <@  uncurry (flip (foldr ap1))

sequence     ::  [Parser s a] -> Parser s [a]
sequence      =  foldr (<:*>) (succeed [])

choice       ::  [Parser s a] -> Parser s a
choice        =  foldr (<|>) failp

chainl' p s   =  q
          where  q = (option (q <*> s) <?@ (id,ap1) ) <*> p <@ ap

chainr' p s   =  q
          where  q = p <*> (option (s <*> q) <?@ (id,ap2) ) <@ ap'
  
-- Parser transformators


just      ::  Parser s a -> Parser s a
just p  =  filter (null.fst) . p

some   ::  Parser s a -> DetPars s a
some p  =  snd . head . just p
 
-- Some common special cases


identifier  ::  Parser Char String
identifier   =  satisfy isAlpha <:*> greedy (satisfy isAlphaNum)

digit       ::  Parser Char Int
digit        =  satisfy isDigit  <@  f
         where  f c = ord c - ord '0'


natural     ::  Parser Char Int
natural      =  greedy1 digit  <@  foldl f 0
         where  f a b = a*10 + b

integer     ::  Parser Char Int
integer      =  option (symbol '-') <*> natural  <@  f
         where  f ([],n) =  n
                f (_ ,n) =  -n

integer'    ::  Parser Char Int
integer'     =  (option (symbol '-') <?@ (id,const negate)) <*> natural  <@ ap

fixed       ::  Parser Char Float
fixed        =  (integer <@ fromIntegral)
                <*> 
                (option (symbol '.' *> fractpart)  <?@  (0.0,id))
                <@  uncurry (+)

fractpart   ::  Parser Char Float
fractpart    =  greedy digit  <@  foldr f 0.0
         where  f d n = (n +  fromIntegral d)/10.0
         
float       ::  Parser Char Float
float        =  fixed 
                <*> 
                (option (symbol 'E' *> integer) <?@ (0,id) )
                <@ f
         where  f (m,e)  =  m * power e
                power e | e<0       = 1.0 / power (-e)
                        | otherwise = fromInteger(10^e)

sp  ::  Parser Char a -> Parser Char a
sp   =  (greedy (satisfy isSpace) *> )  
  
sp'  =  ( . (dropWhile isSpace))


sptoken  ::  String -> Parser Char String
sptoken   =  sp . token

spsymbol ::  Char -> Parser Char Char
spsymbol  =  sp . symbol

spident  ::  Parser Char String
spident   =  sp identifier

parenthesized, bracketed, braced, angled, quoted   :: Parser Char a -> Parser Char a
parenthesized p = pack (spsymbol '(')  p (spsymbol ')')
bracketed p     = pack (spsymbol '[')  p (spsymbol ']')
braced p        = pack (spsymbol '{')  p (spsymbol '}')
angled p        = pack (spsymbol '<')  p (spsymbol '>')
quoted p        = pack (spsymbol '"')  p (spsymbol '"')
stropped p      = pack (spsymbol '\'') p (spsymbol '\'')

commaList, semicList  ::  Parser Char a -> Parser Char [a]
commaList p  =  listOf p (spsymbol ',')
semicList p  =  listOf p (spsymbol ';')

twopass ::  Parser a b -> Parser b c -> Parser a c
twopass lex synt xs = [ (rest,tree)
                      | (rest,tokens) <- many lex xs
                      , (_,tree)      <- just synt tokens
                      ] 
                      
--------------------------------------------   
data LExp = LAMBDA String LExp | 
            ID String | 
            APL LExp LExp |
            CON String | CN Int
            deriving(Show, Read, Eq)

-- lambda	:: Parser Char LExp

-- Type-definition for parsetree

data Expr = Con Int
          | Var String
          | Fun String [Expr]
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          deriving(Show, Read, Eq)
-------------------------------------------------------------
-- Parser for expressions with two priorities

fact :: Parser Char Expr
fact =  integer <@ Con
        <|> identifier <*> (option (parenthesized (commaList expr)) <?@ (Var,flip Fun))
                 <@ ap'
        <|> parenthesized expr

term :: Parser Char Expr
term  =  chainr fact
               (    symbol '*' <@ const (:*:)
                <|> symbol '/' <@ const (:/:)
               )

expr :: Parser Char Expr
expr  =  chainr term
               (    symbol '+' <@ const (:+:)
                <|> symbol '-' <@ const (:-:)
               )


-------------------------------------------------------------
-- Parser for expressions with aribitrary many priorities

type Op a = (Char, a->a->a)

fact' :: Parser Char Expr
fact' =     integer 
            <@ Con
        <|> identifier 
            <*> (option (parenthesized (commaList expr')) <?@ (Var,flip Fun))
            <@ ap'
        <|> parenthesized expr'

gen  ::  [Op a] -> Parser Char a -> Parser Char a
gen ops p  =  chainr p (choice (map f ops))
       where  f (s,c) = symbol s <@ const c

expr' :: Parser Char Expr
expr'  =  foldr gen fact' [addis, multis]

multis  =  [ ('*',(:*:)), ('/',(:/:)) ]
addis   =  [ ('+',(:+:)), ('-',(:-:)) ]
                    
-----------------

data Tree     = Nil | Bin (Tree, Tree) deriving(Show, Read, Eq)

parens  :: Parser Char Tree
parens  = ( symbol '('
            <*> parens
            <*> symbol ')'
            <*> parens
           ) <@ (\(_,(x,(_,y))) -> Bin(x,y))
           <|> epsilon     <@ const Nil

open     = symbol '('
close    = symbol ')'
           
nesting       :: Parser Char Int
nesting       = ( open *> nesting <* close) <*> nesting <@ f
                <|> succeed 0
              where f (x,y) = (1+x) `max` y

foldparens      :: ((a,a)->a) -> a -> Parser Char a
foldparens f e  = p  
           where p = (open *> p <* close) <*> p <@ f <|> succeed e
           