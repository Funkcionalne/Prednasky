-- kodovanie UTF-8 bez BOM (Notepad++)
module Terms where
 
import Data.Char
 
-- identifikator premennej je String 
type Var = String

-- lambda termy
data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp  
            -- CON String | CN Integer 
            deriving(Eq)

instance Show LExp where
  show (LAMBDA v e) = '\\' : v ++ "->" ++ show e
  show (ID v) = v
  show (APP e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
--------------------- useful stuff

izero = (LAMBDA "f" (LAMBDA "x" (ID "x")))
omega = (LAMBDA "x" (APP (ID "x") (ID "x")))
bigOmega = (APP omega omega)
isucc = (LAMBDA "n" 
          (LAMBDA "f" 
            (LAMBDA "x" (APP (ID "f") (APP (APP (ID "n") (ID "f")) (ID "x"))) )))
omega3 = (LAMBDA "x" (APP (APP (ID "x") (ID "x")) (ID "x")))
y = (LAMBDA "f" 
        (APP
          (LAMBDA "x" (APP (ID "f") (APP (ID "x") (ID "x"))))
          (LAMBDA "x" (APP (ID "f") (APP (ID "x") (ID "x"))))
        )  
    )      
            
-- kombinatory S,K,I            
i = (LAMBDA "x" (ID "x"))            
k = (LAMBDA "x" (LAMBDA "y" (ID "x")))            
s = (LAMBDA "x" (LAMBDA "y" (LAMBDA "z" (APP (APP (ID "x") (ID "z")) (APP (ID "y") (ID "z"))))))
            
-- zopar z nich odvodenych konstant            
ione =    (APP isucc izero)
itwo =    (APP isucc (APP isucc izero))
ifour =   (APP isucc (APP isucc (APP isucc (APP isucc izero))))
iquad =   (APP (APP ipower itwo) itwo )
ieight =  (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc (APP isucc izero))))))))
isix =   (APP (APP itimes itwo) ithree )
ithree =  (APP (APP iplus itwo) ione)
inine =   (APP (APP itimes ithree) ithree)
isixteen = (APP (APP ipower itwo) ifour)

itimes00 = (APP (APP itimes izero) izero)
itimes11 = (APP (APP itimes ione) ione)
iplus11 = (APP (APP iplus ione) ione)

iplus =  fst $ fromString "\\m->\\n->\\f->\\x->((m f) ((n f) x))" 
itimes = fst $ fromString "\\m->\\n->\\f->\\x->((m (n f)) x)"
ipower = fst $ fromString "\\m->\\n->(n m)"  

 -- λz. ((λy. y (λx. x)) (λx. z x))
foo =   (LAMBDA "z"
         (APP
            (LAMBDA "y" (APP (ID "y") (LAMBDA "x" (ID "x"))))
            (LAMBDA "x" (APP (ID "z") (ID "x")))
         )
         )
         
-- nie je uzavrety term...
-- (λx.λy.((z x) (λu.(u x)))) (λx.(w x))         
goo = (APP
        (LAMBDA "x" (LAMBDA "y" 
            (APP 
                (APP (ID "z") (ID "x"))
                (LAMBDA "u" (APP (ID "u") (ID "x"))))))
        (LAMBDA "x" (APP (ID "w") (ID "x")))
       )
       
-- λx.λy.y (λz.z x) x       
hoo = (LAMBDA "x" (LAMBDA "y"
                    (APP
                    (APP (ID "y")
                         (LAMBDA "z"
                            (APP (ID "z") (ID "x"))))
                     (ID "x"))))
                     
                     
-- λf.(λx.x x) (λy.y (λz.x))
ioo= (LAMBDA "f" (APP
        (LAMBDA "x" (APP (ID "x") (ID "x")))
        (LAMBDA "y" (APP (ID "y") (LAMBDA "z" (ID "x")))) 
        )
      )




fromString  :: String -> (LExp, String)
fromString (x:xs)   | isAlpha x  = (ID [x], xs)
                    | x == '('  = let (exp1, rest) = fromString xs in
                                    let (exp2, nrest) = fromString (tail rest) in 
                                              (APP exp1 exp2, tail nrest)
                    | x == '\\' = let (exp, rest) = fromString (drop 3 xs) in
                                    (LAMBDA [head xs] exp, rest)
fromString  xs      = error ("syntax error: " ++ xs)                                                   
 


