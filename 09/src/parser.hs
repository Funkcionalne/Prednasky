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

import Prelude hiding ((<*>), (*>), (<*), sequence)
import Data.Char

import Terms -- pre domacu ulohu 1/8

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

-- lambda  :: Parser Char LExp

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
           
big = 
      (token("64205")
      <|> token("65201") 
      <|> token("65202") 
      <|> token("65203") 
      <|> token("65082") 
      <|> token("65250") 
      <|> token("65030") 
      <|> token("78739") 
      <|> token("78737") 
      <|> token("78741") 
      <|> token("6240") 
      <|> token("6241") 
      <|> token("6370") 
      <|> token("6280") 
      <|> token("6343") 
      <|> token("6344") 
      <|> token("1311") 
      <|> token("1317") 
      <|> token("1480") 
      <|> token("1359") 
      <|> token("1473") 
      <|> token("1452") 
      <|> token("1375") 
      <|> token("1378") 
      <|> token("1384") 
      <|> token("1492") 
      <|> token("1493") 
      <|> token("1415") 
      <|> token("1416") 
      <|> token("1321") 
      <|> token("1025") 
      <|> token("1026") 
      <|> token("1027") 
      <|> token("1257") 
      <|> token("1271") 
      <|> token("1397") 
      <|> token("44454") 
      <|> token("44434") 
      <|> token("91822") 
      <|> token("2432") 
      <|> token("11816") 
      <|> token("11916") 
      <|> token("11978") 
      <|> token("11918") 
      <|> token("11826")
      <|> token("11880") 
      <|> token("11968") 
      <|> token("11927") 
      <|> token("11855") 
      <|> token("11930") 
      <|> token("11952") 
      <|> token("11938") 
      <|> token("11993") 
      <|> token("11958") 
      <|> token("11903") 
      <|> token("11976") 
      <|> token("11934") 
      <|> token("11867") 
      <|> token("11933") 
      <|> token("11841") 
      <|> token("11858") 
      <|> token("41216") 
      <|> token("41218") 
      <|> token("41194") 
      <|> token("41184") 
      <|> token("41196") 
      <|> token("40914") 
      <|> token("40970") 
      <|> token("40968") 
      <|> token("40938") 
      <|> token("40954") 
      <|> token("40948") 
      <|> token("40990") 
      <|> token("40911") 
      <|> token("40913") 
      <|> token("78862") 
      <|> token("13611") 
      <|> token("13629") 
      <|> token("13600") 
      <|> token("13615") 
      <|> token("13622") 
      <|> token("37686") 
      <|> token("37704") 
      <|> token("37788") 
      <|> token("66310") 
      <|> token("66104") 
      <|> token("66318") 
      <|> token("66325") 
      <|> token("66305") 
      <|> token("66160") 
      <|> token("66390") 
      <|> token("66215") 
      <|> token("66422") 
      <|> token("66120") 
      <|> token("87582") 
      <|> token("87344") 
      <|> token("87593") 
      <|> token("87692") 
      <|> token("87418") 
      <|> token("87480") 
      <|> token("87047") 
      <|> token("87121") 
      <|> token("87371") 
      <|> token("91765") 
      <|> token("11103") 
      <|> token("11240") 
      <|> token("11238") 
      <|> token("11290") 
      <|> token("11120") 
      <|> token("11310") 
      <|> token("11336") 
      <|> token("11231") 
      <|> token("11010") 
      <|> token("11060") 
      <|> token("11330") 
      <|> token("11149") 
      <|> token("11150") 
      <|> token("11350") 
      <|> token("11389") 
      <|> token("11109") 
      <|> token("11036") 
      <|> token("11035") 
      <|> token("11034") 
      <|> token("11040") 
      <|> token("11080") 
      <|> token("11090") 
      <|> token("11213") 
      <|> token("11059") 
      <|> token("11144") 
      <|> token("94672") 
      <|> token("94896") 
      <|> token("94578") 
      <|> token("94287") 
      <|> token("94926") 
      <|> token("94120") 
      <|> token("94856") 
      <|> token("94580") 
      <|> token("94975") 
      <|> token("94968") 
      <|> token("94866") 
      <|> token("94776") 
      <|> token("94610") 
      <|> token("94569") 
      <|> token("94767") 
      <|> token("95551") 
      <|> token("94294") 
      <|> token("95748") 
      <|> token("13242") 
      <|> token("13228") 
      <|> token("14544") 
      <|> token("13348") 
      <|> token("13353") 
      <|> token("13257") 
      <|> token("14549") 
      <|> token("78954") 
      <|> token("41978") 
      <|> token("41923") 
      <|> token("41947") 
      <|> token("06450") 
      <|> token("06451") 
      <|> token("06449") 
      <|> token("06431") 
      <|> token("06478") 
      <|> token("65510") 
      <|> token("65505") 
      <|> token("65503") 
      <|> token("65502") 
      <|> token("15655") 
      <|> token("15526") 
      <|> token("15528") 
      <|> token("15625") 
      <|> token("15626") 
      <|> token("15640") 
      <|> token("15614") 
      <|> token("15552") 
      <|> token("64390") 
      <|> token("64397") 
      <|> token("65338") 
      <|> token("65344") 
      <|> token("65330") 
      <|> token("95323") 
      <|> token("85223") 
      <|> token("85201") 
      <|> token("85154") 
      <|> token("85242") 
      <|> token("85293") 
      <|> token("85245") 
      <|> token("85283") 
      <|> token("85364") 
      <|> token("85365") 
      <|> token("81680") 
      <|> token("83583") 
      <|> token("86715") 
      <|> token("86933") 
      <|> token("81758") 
      <|> token("82331") 
      <|> token("82332") 
      <|> token("82899") 
      <|> token("82900") 
      <|> token("83755") 
      <|> token("83743") 
      <|> token("86678") 
      <|> token("83780") 
      <|> token("83781") 
      <|> token("78062") 
      <|> token("78073") 
      <|> token("68054") 
      <|> token("68244") 
      <|> token("68148") 
      <|> token("68032") 
      <|> token("68070") 
      <|> token("68086") 
      <|> token("26941") 
      <|> token("33008") 
      <|> token("26850") 
      <|> token("33019") 
      <|> token("26666") 
      <|> token("71877") 
      <|> token("71879") 
      <|> token("71183") 
      <|> token("71628") 
      <|> token("71714") 
      <|> token("71624") 
      <|> token("71892") 
      <|> token("71579") 
      <|> token("64180") 
      <|> token("64235") 
      <|> token("64210") 
      <|> token("64211") 
      <|> token("64220") 
      <|> token("64040") 
      <|> token("64328") 
      <|> token("64360") 
      <|> token("64370") 
      <|> token("64247") 
      <|> token("64228") 
      <|> token("64660") 
      <|> token("64650") 
      <|> token("64600") 
      <|> token("64655") 
      <|> token("64450") 
      <|> token("64401") 
      <|> token("64400") 
      <|> token("6601") 
      <|> token("6630") 
      <|> token("6631") 
      <|> token("6901") 
      <|> token("6700") 
      <|> token("6907") 
      <|> token("6650") 
      <|> token("6681") 
      <|> token("6909") 
      <|> token("6660") 
      <|> token("6664") 
      <|> token("6670") 
      <|> token("65578") 
      <|> token("65555") 
      <|> token("65563") 
      <|> token("85442") 
      <|> token("85418") 
      <|> token("85574") 
      <|> token("85743") 
      <|> token("64892") 
      <|> token("64930") 
      <|> token("64910") 
      <|> token("64860") 
      <|> token("64851") 
      <|> token("64870") 
      <|> token("64950") 
      <|> token("54511") 
      <|> token("56294") 
      <|> token("57516") 
      <|> token("45007") 
      <|> token("57091") 
      <|> token("57411") 
      <|> token("58362") 
      <|> token("58367") 
      <|> token("57494") 
      <|> token("57584") 
      <|> token("57290") 
      <|> token("80028") 
      <|> token("80222") 
      <|> token("80259") 
      <|> token("80022") 
      <|> token("80097") 
      <|> token("80110") 
      <|> token("78762") 
      <|> token("78774") 
      <|> token("78767") 
      <|> token("78764") 
      <|> token("13455") 
      <|> token("13459") 
      <|> token("13363") 
      <|> token("13462") 
      <|> token("13463") 
      <|> token("13457") 
      <|> token("13464") 
      <|> token("78255") 
      <|> token("78339") 
      <|> token("78368") 
      <|> token("78267") 
      <|> token("78224") 
      <|> token("78373") 
      <|> token("78264") 
      <|> token("78229") 
      <|> token("8583") 
      <|> token("8589") 
      <|> token("8594") 
      <|> token("17609") 
      <|> token("17601") 
      <|> token("17607") 
      <|> token("17600") 
      <|> token("11723") 
      <|> token("11722") 
      <|> token("11546") 
      <|> token("11541") 
      <|> token("11406") 
      <|> token("11648") 
      <|> token("11649") 
      <|> token("11414") 
      <|> token("11603") 
      <|> token("11418") 
      <|> token("11782") 
      <|> token("11652") 
      <|> token("11643") 
      <|> token("11450") 
      <|> token("11448") 
      <|> token("11518") 
      <|> token("11520") 
      <|> token("11567") 
      <|> token("11748") 
      <|> token("11747") 
      <|> token("11502") 
      <|> token("11679") 
      <|> token("10381") 
      <|> token("10382") 
      <|> token("10384") 
      <|> token("10385") 
      <|> token("10513") 
      <|> token("10517") 
      <|> token("10518") 
      <|> token("10519") 
      <|> token("10224") 
      <|> token("10416") 
      <|> token("10488") 
      <|> token("10486") 
      <|> token("10487") 
      <|> token("10400") 
      <|> token("10410") 
      <|> token("10409") 
      <|> token("10412") 
      <|> token("10637") 
      <|> token("10630") 
      <|> token("10141") 
      <|> token("10147") 
      <|> token("10148") 
      <|> token("10149") 
      <|> token("10338") 
      <|> token("10339") 
      <|> token("10469") 
      <|> token("10471") 
      <|> token("10729") 
      <|> token("10730") 
      <|> token("10865") 
      <|> token("10866") 
      <|> token("10868") 
      <|> token("10870") 
      <|> token("10763") 
      <|> token("10737") 
      <|> token("10738") 
      <|> token("10739") 
      <|> token("03091") 
      <|> token("03534") 
      <|> token("03726") 
      <|> token("03220") 
      <|> token("03160") 
      <|> token("03839") 
      <|> token("03140") 
      <|> token("03522") 
      <|> token("03059") 
      <|> token("03347") 
      <|> token("03334") 
      <|> token("03492") 
      <|> token("03827") 
      <|> token("03865") 
      <|> token("37484") 
      <|> token("37531") 
      <|> token("37395") 
      <|> token("37385") 
      <|> token("37549") 
      <|> token("37545") 
      <|> token("37279") 
      <|> token("65472") 
      <|> token("65442") 
      <|> token("65418") 
      <|> token("61701") 
      <|> token("61832") 
      <|> token("61831") 
      <|> token("61829") 
      <|> token("61818") 
      <|> token("64820") 
      <|> token("64810") 
      <|> token("16741") 
      <|> token("16765") 
      <|> token("16685") 
      <|> token("16742") 
      <|> token("16648") 
      <|> token("16732") 
      <|> token("16689") 
      <|> token("16717") 
      <|> token("16723") 
      <|> token("16622") 
      <|> token("16719") 
      <|> token("78640") 
      <|> token("78641") 
      <|> token("78629") 
      <|> token("61781") 
      <|> token("61766") 
      <|> token("81001") 
      <|> token("81058") 
      <|> token("78708") 
      <|> token("78720") 
      <|> token("14472") 
      <|> token("14474") 
      <|> token("14447") 
      <|> token("14232") 
      <|> token("14454") 
      <|> token("14314") 
      <|> token("14280") 
      <|> token("14301") 
      <|> token("14307") 
      <|> token("14321") 
      <|> token("13116") 
      <|> token("14303") 
      <|> token("14323") 
      <|> token("14438") 
      <|> token("13150") 
      <|> token("13334") 
      <|> token("14431") 
      <|> token("14236") 
      <|> token("14240") 
      <|> token("14241") 
      <|> token("78435") 
      <|> token("78439") 
      <|> token("12839") 
      <|> token("12840") 
      <|> token("12843") 
      <|> token("12882") 
      <|> token("12822") 
      <|> token("12772") 
      <|> token("12942") 
      <|> token("12935") 
      <|> token("12982") 
      <|> token("96781") 
      <|> token("96783") 
      <|> token("96741") 
      <|> token("96745") 
      <|> token("96747") 
      <|> token("96749") 
      <|> token("96035") 
      <|> token("96221") 
      <|> token("96837") 
      <|> token("96839") 
      <|> token("96935") 
      <|> token("03955") 
      <|> token("03969") 
      <|> token("03964") 
      <|> token("40186") 
      <|> token("40155") 
      <|> token("40183") 
      <|> token("10184") 
      <|> token("42647") 
      <|> token("42807") 
      <|> token("42809") 
      <|> token("42181") 
      <|> token("42182") 
      <|> token("43128") 
      <|> token("42366") 
      <|> token("42367") 
      <|> token("43002") 
      <|> token("43003") 
      <|> token("42840") 
      <|> token("40689") 
      <|> token("40688") 
      <|> token("40670") 
      <|> token("40623") 
      <|> token("40650") 
      <|> token("40621") 
      <|> token("40608") 
      <|> token("40766") 
      <|> token("40745") 
      <|> token("40848") 
      <|> token("40706") 
      <|> token("40754") 
      <|> token("04030") 
      <|> token("16190") 
      <|> token("16270") 
      <|> token("16132") 
      <|> token("16459") 
      <|> token("16365") 
      <|> token("16171") 
      <|> token("16170") 
      <|> token("16121") 
      <|> token("16120") 
      <|> token("16206") 
      <|> token("16066") 
      <|> token("16289") 
      <|> token("16405") 
      <|> token("16181") 
      <|> token("16149") 
      <|> token("16235") 
      <|> token("16238") 
      <|> token("16239") 
      <|> token("16059") 
      <|> token("16060") 
      <|> token("16061") 
      <|> token("16105") 
      <|> token("16090") 
      <|> token("16266") 
      <|> token("93119") 
      <|> token("93780") 
      <|> token("93890") 
      <|> token("93172") 
      <|> token("93132") 
      <|> token("93186") 
      <|> token("93436") 
      <|> token("41256") 
      <|> token("41316") 
      <|> token("78793") 
      <|> token("78792") 
      <|> token("84752") 
      <|> token("84452") 
      <|> token("84531") 
      <|> token("84377") 
      <|> token("84628") 
      <|> token("84401") 
      <|> token("84501") 
      <|> token("91938") 
      <|> token("91939") 
      <|> token("92035") 
      <|> token("98753") 
      <|> token("98425") 
      <|> token("98836") 
      <|> token("41630") 
      <|> token("41596") 
      <|> token("41764") 
      <|> token("41571") 
      <|> token("41780") 
      <|> token("41640") 
      <|> token("41675") 
      <|> token("41530") 
      <|> token("41660") 
      <|> token("12240") 
      <|> token("12150") 
      <|> token("12140") 
      <|> token("12560") 
      <|> token("12566") 
      <|> token("12495") 
      <|> token("12330") 
      <|> token("12205") 
      <|> token("12372") 
      <|> token("12375") 
      <|> token("12424") 
      <|> token("12425") 
      <|> token("12625") 
      <|> token("12465") 
      <|> token("08578") 
      <|> token("08549") 
      <|> token("08554") 
      <|> token("08521") 
      <|> token("08536") 
      <|> token("08535") 
      <|> token("08545") 
      <|> token("86218") 
      <|> token("86216") 
      <|> token("41170") 
      <|> token("61980") 
      <|> token("61984") 
      <|> token("15333") 
      <|> token("15300") 
      <|> token("15421") 
      <|> token("15422") 
      <|> token("15120") 
      <|> token("15480") 
      <|> token("15450") 
      <|> token("15310") 
      <|> token("15090") 
      <|> token("15377") 
      <|> token("15282") 
      <|> token("15302") 
      <|> token("15325") 
      <|> token("15247") 
      <|> token("13272") 
      <|> token("13274") 
      <|> token("13388") 
      <|> token("13168") 
      <|> token("13473") 
      <|> token("30710") 
      <|> token("29570") 
      <|> token("27459") 
      <|> token("29634") 
      <|> token("34731") 
      <|> token("26063") 
      <|> token("28900") 
      <|> token("34171") 
      <|> token("28722") 
      <|> token("31960") 
      <|> token("34467") 
      <|> token("34122") 
      <|> token("64384") 
      <|> token("64387") 
      <|> token("40415") 
      <|> token("40419") 
      <|> token("41036") 
      <|> token("41114") 
      <|> token("40375") 
      <|> token("91507") 
      <|> token("91517") 
      <|> token("91520") 
      <|> token("63994") 
      <|> token("62805") 
      <|> token("62760") 
      <|> token("62771") 
      <|> token("62941") 
      <|> token("62730") 
      <|> token("62721") 
      <|> token("62772") 
      <|> token("62641") 
      <|> token("62751") 
      <|> token("02217") 
      <|> token("02512") 
      <|> token("02513") 
      <|> token("02611") 
      <|> token("02562") 
      <|> token("02635") 
      <|> token("02636") 
      <|> token("02460") 
      <|> token("02462") 
      <|> token("02446") 
      <|> token("48698") 
      <|> token("14023") 
      <|> token("14014") 
      <|> token("14015") 
      <|> token("14025") 
      <|> token("14026") 
      <|> token("61881") 
      <|> token("61641") 
      <|> token("61600") 
      <|> token("63160") 
      <|> token("63270") 
      <|> token("63170") 
      <|> token("63260") 
      <|> token("81202") 
      <|> token("78662") 
      <|> token("78663") 
      <|> token("78655") 
      <|> token("40007") 
      <|> token("40080") 
      <|> token("40030") 
      <|> token("68399") 
      <|> token("68396") 
      <|> token("68391") 
      <|> token("64756") 
      <|> token("64754") 
      <|> token("64758") 
      <|> token("64706") 
      <|> token("64700") 
      <|> token("64709") 
      <|> token("64750") 
      <|> token("65357") 
      <|> token("65387") 
      <|> token("65361") 
      <|> token("48454") 
      <|> token("48455") 
      <|> token("48327") 
      <|> token("48459") 
      <|> token("48569") 
      <|> token("48431") 
      <|> token("48564") 
      <|> token("48565") 
      <|> token("48420") 
      <|> token("48354") 
      <|> token("38836") 
      <|> token("91720") 
      <|> token("91723") 
      <|> token("97390") 
      <|> token("38880") 
      <|> token("60714") 
      <|> token("60769") 
      <|> token("60765") 
      <|> token("60767") 
      <|> token("60735") 
      <|> token("60715") 
      <|> token("60742") 
      <|> token("60740") 
      <|> token("60728") 
      <|> token("60750") 
      <|> token("60710") 
      <|> token("91789") 
      <|> token("17350") 
      <|> token("17310") 
      <|> token("17128") 
      <|> token("17129") 
      <|> token("17130") 
      <|> token("17300") 
      <|> token("17175") 
      <|> token("17295") 
      <|> token("17116") 
      <|> token("17280") 
      <|> token("17296") 
      <|> token("17260") 
      <|> token("17060") 
      <|> token("17062") 
      <|> token("17218") 
      <|> token("17244") 
      <|> token("17232") 
      <|> token("17298") 
      <|> token("78969") 
      <|> token("91643") 
      <|> token("46740") 
      <|> token("64751") 
      <|> token("46696") 
      <|> token("63789") 
      <|> token("63894") 
      <|> token("63862") 
      <|> token("63801") 
      <|> token("63932") 
      <|> token("63866") 
      <|> token("63790") 
      <|> token("63756") 
      <|> token("63844") 
      <|> token("63870") 
      <|> token("34504") 
      <|> token("34519") 
      <|> token("34300") 
      <|> token("33345") 
      <|> token("33393") 
      <|> token("33837") 
      <|> token("34601") 
      <|> token("63630") 
      <|> token("63680") 
      <|> token("70273") 
      <|> token("72219") 
      <|> token("72254") 
      <|> token("72406") 
      <|> token("72509") 
      <|> token("72530") 
      <|> token("72428") 
      <|> token("72259") 
      <|> token("72565") 
      <|> token("72537") 
      <|> token("91182") 
      <|> token("72243") 
      <|> token("72438") 
      <|> token("72206") 
      <|> token("72386") 
      <|> token("72295") 
      <|> token("72334") 
      <|> token("72202") 
      <|> token("72658") 
      <|> token("72231") 
      <|> token("72503") 
      <|> token("72353") 
      <|> token("72408") 
      <|> token("72278") 
      <|> token("72520") 
      <|> token("72483") 
      <|> token("72253") 
      <|> token("72290") 
      <|> token("72494") 
      <|> token("72793") 
      <|> token("72434") 
      <|> token("72405") 
      <|> token("86430") 
      <|> token("86360") 
      <|> token("38611") 
      <|> token("38264") 
      <|> token("38457") 
      <|> token("80410") 
      <|> token("80415") 
      <|> token("80407") 
      <|> token("80413") 
      <|> token("80472") 
      <|> token("48820") 
      <|> token("48825") 
      <|> token("48900") 
      <|> token("48855") 
      <|> token("91742") 
      <|> token("41404") 
      <|> token("41466") 
      <|> token("68816") 
      <|> token("68588") 
      <|> token("68858") 
      <|> token("68368") 
      <|> token("68581") 
      <|> token("68842") 
      <|> token("68262") 
      <|> token("67581") 
      <|> token("67663") 
      <|> token("67475") 
      <|> token("67743") 
      <|> token("67666") 
      <|> token("67665") 
      <|> token("67561") 
      <|> token("67965") 
      <|> token("67867") 
      <|> token("67775") 
      <|> token("63125") 
      <|> token("63145") 
      <|> token("06030") 
      <|> token("06070") 
      <|> token("06074") 
      <|> token("06170") 
      <|> token("06180") 
      <|> token("06181") 
      <|> token("06186") 
      <|> token("06187") 
      <|> token("06080") 
      <|> token("06102") 
      <|> token("06108") 
      <|> token("06120") 
      <|> token("78905") 
      <|> token("78906") 
      <|> token("78482") 
      <|> token("78488") 
      <|> token("78458") 
      <|> token("78479") 
      <|> token("78460") 
      <|> token("78486") 
      <|> token("78485") 
      <|> token("60390") 
      <|> token("60360") 
      <|> token("60468") 
      <|> token("60401") 
      <|> token("60402") 
      <|> token("60419") 
      <|> token("60425") 
      <|> token("60461") 
      <|> token("60481") 
      <|> token("60490") 
      <|> token("60520") 
      <|> token("84239") 
      <|> token("84203") 
      <|> token("84204") 
      <|> token("84071") 
      <|> token("26215") 
      <|> token("26058") 
      <|> token("26231") 
      <|> token("26038") 
      <|> token("26242") 
      <|> token("26233") 
      <|> token("62315") 
      <|> token("62318") 
      <|> token("62392") 
      <|> token("62393") 
      <|> token("62366") 
      <|> token("62371") 
      <|> token("62467") 
      <|> token("62463") 
      <|> token("62464") 
      <|> token("62405") 
      <|> token("62332") 
      <|> token("62333") 
      <|> token("62460") 
      <|> token("62450") 
      <|> token("62456") 
      <|> token("60096") 
      <|> token("60033") 
      <|> token("63021") 
      <|> token("63023") 
      <|> token("08181") 
      <|> token("08190") 
      <|> token("08025") 
      <|> token("08373") 
      <|> token("60030") 
      <|> token("08220") 
      <|> token("08221") 
      <|> token("08222") 
      <|> token("08223") 
      <|> token("08224") 
      <|> token("08227") 
      <|> token("08482") 
      <|> token("08429") 
      <|> token("08430") 
      <|> token("08433") 
      <|> token("08301") 
      <|> token("08302") 
      <|> token("08306") 
      <|> token("08391") 
      <|> token("08284") 
      <|> token("08285") 
      <|> token("08160") 
      <|> token("63450") 
      <|> token("63460") 
      <|> token("63332") 
      <|> token("63451") 
      <|> token("63333") 
      <|> token("63471") 
      <|> token("63331") 
      <|> token("63402") 
      <|> token("63330") 
      <|> token("02703") 
      <|> token("02985") 
      <|> token("02986") 
      <|> token("02795") 
      <|> token("02934") 
      <|> token("02974") 
      <|> token("02975") 
      <|> token("02978") 
      <|> token("02989") 
      <|> token("02998") 
      <|> token("02935") 
      <|> token("02732") 
      <|> token("02917") 
      <|> token("02955") 
      <|> token("02965") 
      <|> token("02851") 
      <|> token("02875") 
      <|> token("02876") 
      <|> token("02751") 
      <|> token("02926") 
      <|> token("02952") 
      <|> token("02760") 
      <|> token("02943") 
      <|> token("02944") 
      <|> token("02744") 
      <|> token("02763") 
      <|> token("02747") 
      <|> token("02773") 
      <|> token("02972") 
      <|> token("91656") 
      <|> token("91657") 
      <|> token("91680") 
      <|> token("91689") 
      <|> token("91334") 
      <|> token("07761") 
      <|> token("07602") 
      <|> token("07510") 
      <|> token("07754") 
      <|> token("07684") 
      <|> token("07469") 
      <|> token("07499") 
      <|> token("07031") 
      <|> token("07125") 
      <|> token("07667") 
      <|> token("07316") 
      <|> token("07003") 
      <|> token("07015") 
      <|> token("07480") 
      <|> token("07481") 
      <|> token("07650") 
      <|> token("07643") 
      <|> token("07222") 
      <|> token("07690") 
      <|> token("07149") 
      <|> token("07150") 
      <|> token("07156") 
      <|> token("07157") 
      <|> token("07190") 
      <|> token("07660") 
      <|> token("07678") 
      <|> token("07629") 
      <|> token("07630") 
      <|> token("07210") 
      <|> token("64570") 
      <|> token("64500") 
      <|> token("64501") 
      <|> token("03222") 
      <|> token("03166") 
      <|> token("03134") 
      <|> token("03770") 
      <|> token("03772") 
      <|> token("03776") 
      <|> token("03779") 
      <|> token("16242") 
      <|> token("78397") 
      <|> token("40270") 
      <|> token("40255") 
      <|> token("47808") 
      <|> token("47770") 
      <|> token("47759") 
      <|> token("47635") 
      <|> token("47771") 
      <|> token("47412") 
      <|> token("47662") 
      <|> token("47671") 
      <|> token("47686") 
      <|> token("47687") 
      <|> token("47695") 
      <|> token("47670") 
      <|> token("63686") 
      <|> token("63723") 
      <|> token("63687") 
      <|> token("63708") 
      <|> token("63661") 
      <|> token("63799") 
      <|> token("63820") 
      <|> token("63740") 
      <|> token("63714") 
      <|> token("38353") 
      <|> token("38615") 
      <|> token("36911") 
      <|> token("48962") 
      <|> token("48969") 
      <|> token("48983") 
      <|> token("48991") 
      <|> token("48966") 
      <|> token("91610") 
      <|> token("67001") 
      <|> token("78857") 
      <|> token("78859") 
      <|> token("47041") 
      <|> token("47070") 
      <|> token("47060") 
      <|> token("47058") 
      <|> token("47055") 
      <|> token("47112") 
      <|> token("47156") 
      <|> token("47159") 
      <|> token("47108") 
      <|> token("47142") 
      <|> token("47132") 
      <|> token("40581") 
      <|> token("36870") 
      <|> token("35394") 
      <|> token("36003") 
      <|> token("38328") 
      <|> token("38341") 
      <|> token("48955") 
      <|> token("48947") 
      <|> token("48940") 
      <|> token("48928") 
      <|> token("40100") 
      <|> token("40103") 
      <|> token("06990") 
      <|> token("43466") 
      <|> token("68456") 
      <|> token("26629") 
      <|> token("26509") 
      <|> token("26529") 
      <|> token("26524") 
      <|> token("26730") 
      <|> token("06590") 
      <|> token("26544") 
      <|> token("26425") 
      <|> token("26406") 
      <|> token("26422") 
      <|> token("62124") 
      <|> token("62053") 
      <|> token("62016") 
      <|> token("62019") 
      <|> token("62010") 
      <|> token("60250") 
      <|> token("60252") 
      <|> token("60155") 
      <|> token("60141") 
      <|> token("60120") 
      <|> token("60230") 
      <|> token("60150") 
      <|> token("60115") 
      <|> token("60135") 
      <|> token("60100") 
      <|> token("60101") 
      <|> token("33745") 
      <|> token("33815") 
      <|> token("33754") 
      <|> token("33829") 
      <|> token("67083") 
      <|> token("67085") 
      <|> token("67137") 
      <|> token("67027") 
      <|> token("67095") 
      <|> token("67161") 
      <|> token("91376") 
      <|> token("13583") 
      <|> token("13589") 
      <|> token("13586") 
      <|> token("13588") 
      <|> token("61291") 
      <|> token("61265") 
      <|> token("61297") 
      <|> token("48093") 
      <|> token("48042") 
      <|> token("48103") 
      <|> token("48094") 
      <|> token("48097") 
      <|> token("44214") 
      <|> token("44287") 
      <|> token("44218") 
      <|> token("44231") 
      <|> token("44292") 
      <|> token("44212") 
      <|> token("61415") 
      <|> token("61442") 
      <|> token("16597") 
      <|> token("61995") 
      <|> token("43555") 
      <|> token("67586") 
      <|> token("76595") 
      <|> token("76612") 
      <|> token("76679") 
      <|> token("76680") 
      <|> token("76393") 
      <|> token("76394") 
      <|> token("76685") 
      <|> token("48625") 
      <|> token("48679") 
      <|> token("48647") 
      <|> token("48648") 
      <|> token("48650") 
      <|> token("96413") 
      <|> token("67297") 
      <|> token("67295") 
      <|> token("67339") 
      <|> token("67341") 
      <|> token("67237") 
      <|> token("67283") 
      <|> token("67261") 
      <|> token("67335") 
      <|> token("68018") 
      <|> token("68098") 
      <|> token("68104") 
      <|> token("68110") 
      <|> token("91592") 
      <|> token("61024") 
      <|> token("61080") 
      <|> token("61052") 
      <|> token("61043") 
      <|> token("61090") 
      <|> token("94996") 
      <|> token("65125") 
      <|> token("65229") 
      <|> token("65208") 
      <|> token("65019") 
      <|> token("65046") 
       )
           