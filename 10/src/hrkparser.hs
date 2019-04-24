module HrkParser where

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language
--import Text.ParserCombinators.Parsec.Language(haskellStyle)
--import Text.ParserCombinators.Parsec.Language(javaStyle)

run  :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
      Left err -> do { putStr "parse error at" ; print err }    
      Right x -> print x

-- run (char '!') "!123!"
-- run (oneOf "!?.") "?123"
-- run letter "a"
-- run letter "123"
-- run digit "123"
-- run word "abc def"
-- run word "abc123"

paren :: Parser ()    -- parser, ktory nevracia ziadnu hodnotu
paren = do{ char '('
    ; paren
    ; char ')'
    ; paren
    }
         <|> return ()

         
-- run paren "(())"
-- run paren "(()"

data Bin    = Nil | Node Bin Bin    -- vnútorná reprezentácia
    deriving(Show, Read, Eq) 
        
parenBin       :: Parser Bin         -- analyzátor zo String do Bin
parenBin       = do { char '('; x<-parenBin; char ')'; y<-parenBin; return (Node x y) } 
           <|>
         return Nil
{-
run parenBin "(())()"
Node (Node Nil Nil) (Node Nil Nil)

run parenBin "(())"
Node (Node Nil Nil) Nil
-}
         

nesting :: Parser Int    -- parser, ktory vracia hlbku vyrazu
nesting = do{ char '('
    ; n <- nesting
    ; char ')'
    ; m <- nesting
    ; return (max (n+1) m)
    }
           <|> return 0

-- run nesting "(())()"
-- run nesting "(()))"

word :: Parser String
word = do{ c <- letter ; 
    do{ cs <- word ; return (c:cs) }
    <|> 
    return [c]
          }

separator :: Parser ()
separator = skipMany1 (space <|> char ',' <|> char ';')
          
-- run (do {separator;word}) ";, sdsd"

sentence :: Parser [String]
sentence = do{ words <- sepBy1 word separator
        ; oneOf ".?!"
        ; return words
    }

number :: Parser Integer
number = do{ ds <- many1 digit ; return (read ds) }
          <?> "number"
    
--------------------------------

lexer :: TokenParser ()
lexer = makeTokenParser(
    emptyDef
    { commentStart = "{-"
                   , commentEnd = "-}"
                  , identStart = letter
                  , identLetter = alphaNum
                  , opStart = oneOf "!&|"
                  , opLetter = oneOf "!&|"
                  , reservedOpNames = ["!", "&&", "||"]
                  , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "end",
                                 "while", "do"]
                   }    
          )
    
    
_parens = parens lexer
_semi = semi lexer
_identifier= identifier lexer
_reserved = reserved lexer
_reservedOp= reservedOp lexer
_whiteSpace = whiteSpace lexer
_lexeme = lexeme lexer
_symbol = symbol lexer
_natural = natural lexer
_semiSep1 = semiSep1 lexer
    
-----------------------------------
{-
stmt ::=
    while <expr> do <stmt> end while
    if <expr> then <stmt> [else <stmt>] end if
    <id>++
    <id>--
    
expr ::=      <id> hrka
     <id> nehrka
     true
     false
     <expr> && <expr>
     <expr> || <expr>
     !<expr> 
     (<expr>)
-}

data HrkaNehrka = Hrka | Nehrka    deriving Show
data Expr =     Var String HrkaNehrka | 
    Con Bool | 
    UnOp UnOp Expr | 
    BinOp BinOp Expr Expr
         deriving Show

data UnOp = Not deriving Show
data BinOp = And | Or deriving Show

hrkExpr :: Parser Expr
hrkExpr = buildExpressionParser table factor <?> "hrk-expression"
table = [ [Prefix ( do { _reservedOp "!"; return (UnOp Not)})]
        , [Infix (do { _reservedOp "&&" ; return (BinOp And) }) AssocLeft]
        , [Infix (do { _reservedOp "||" ; return (BinOp Or) }) AssocLeft]
        ]
        
factor = _parens hrkExpr
       <|> (do { id <-_identifier ; 
           kind <- do { _reserved "hrka"; return Hrka } <|> do { _reserved "nehrka"; return Nehrka };
           return (Var id kind)})
       <|> (do { _reserved "true" ; return (Con True)})
       <|> (do { _reserved "false" ; return (Con False)})


{-       
run hrkExpr "(A nehrka) && (B hrka)"
BinOp And (Var "A" Nehrka) (Var "B" Hrka)

Main> run hrkExpr "((A nehrka) && (B hrka))"
BinOp And (Var "A" Nehrka) (Var "B" Hrka)

Main> run hrkExpr "!((A nehrka) && (B hrka))"
UnOp Not (BinOp And (Var "A" Nehrka) (Var "B" Hrka))
-}
       
data Stmt = Inc String | Dec String | If Expr Stmt Stmt | While Expr Stmt | Seq [Stmt]
     deriving Show

hrkStat :: Parser Stmt
hrkStat = do { _whiteSpace;  s<-stmtparser; eof; return s}
     where
       stmtparser :: Parser Stmt
       stmtparser = fmap Seq (_semiSep1 stmt1)
       stmt1 = 
               do { v <- _identifier
                      ; do { _reservedOp "++"; return (Inc v) } <|> do { _reservedOp "--"; return (Dec v) }
                      }
               <|> do { _reserved "if"
                      ; b <- hrkExpr
                      ; _reserved "then"
                      ; p <- stmtparser
                      ; _reserved "else"
                      ; q <- stmtparser
                      ; _reserved "end"
                      ; _reserved "if"
                      ; return (If b p q)
                      }
               <|> do { _reserved "while"
                      ; b <- hrkExpr
                      ; _reserved "do"
                      ; p <- stmtparser
                      ; _reserved "end"
                      ; _reserved "while"
                      ; return (While b p)
                      }

                       