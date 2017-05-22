data Expr = Function String Expr Expr | Variable String deriving(Eq)

type Equation = (Expr, Expr)
type Dependency = (String, Expr)

instance Show Expr
	where
		show (Variable s) = s
		show (Function f e1 e2) = f ++ "(" ++ (show e1) ++ "," ++ (show e2) ++ ")"


contains :: Expr -> String -> Bool
contains _ _ = False


substitute :: String -> Expr -> Expr -> Expr
substitute _ _ e = e


maybeCons :: a -> Maybe [a] -> Maybe [a]
maybeCons _ _ = Nothing


unify :: [Equation] -> Maybe [Dependency]
unify _ = Nothing
