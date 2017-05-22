module Unify where

data Type        = TVar Int | TApl Type Type deriving (Eq)
type Constraint  = (Type,Type)
type Constraints = [Constraint]
 
instance Show Type
  where
    show (TVar s) = show s
    show (TApl e1 e2) = "(" ++ (show e1) ++ "->" ++ (show e2) ++ ")"
    
-- je premenna s indexom Int v typovom vyraze?
contains :: Type -> Int -> Bool
contains (TVar v) x        = (v == x)
contains (TApl t1 t2) x    = (contains t1 x) || (contains t2 x)
 
-- substitucia premenna, co, kam 
substitute :: Int -> Type -> Type -> Type
substitute v new tv@(TVar t)
              | t == v    = new
              | otherwise = tv
substitute v new (TApl t1 t2) = TApl (substitute v new t1) (substitute v new t2)
 
-- redukuje rovnosti, kym nenajde riesenie, resp. zisti, ze neexistuje
unify :: Constraints -> Maybe Constraints

unify []  = return []               -- prazdna mnozina, prazdna substitucia

unify ((TVar v1, TVar v2):subst)    -- v1 = v2
  | v1 == v2      =   unify subst         -- ak su rovnake, taku rovnost ignoruj
  | otherwise     =   do  xs <- (unify (substitute' v1 (TVar v2) subst)) 
                          return ((TVar v1,TVar v2):xs)

unify ((TVar v, e):subst)
  | contains e v == True    =   Nothing    -- occur check
  | otherwise               =   do  xs <-(unify (substitute' v e subst))
                                    return ((TVar v,e):xs)
  
unify ((e, TVar v):subst) = unify ((TVar v, e):subst)  -- prehodi na predosly pripad

unify ((TApl x y, TApl a b):subst) = unify ((x,a):(y,b):subst)
 
----------------------------------------  
-- nahradi vsetko v zozname rovnic, premenna, co, constraints
substitute' :: Int -> Type -> Constraints -> Constraints
substitute' v new le = zip (map (substitute v new) (fst r)) (map (substitute v new) (snd r))
  where r = unzip le  
-- unzip :: [(a, b)] -> ([a], [b])
-- fst r su lave strany
-- snd r su prave strany rovnosti
 
-- prida prvok do Maybe List
add2Maybe :: Constraint -> Maybe Constraints -> Maybe Constraints
add2Maybe a mb = do   xs <- mb
                      return (a:xs)
                      
test1 =  unify [(TVar 0, TVar 1),(TVar 1, (TApl (TVar 2) (TVar 3)))]
test2 = unify [(TVar 0, TVar 1),(TVar 1, (TApl (TVar 1) (TVar 3)))]
test3 =  unify [(TVar 0, TVar 1),(TVar 1, (TApl (TVar 2) (TVar 3))),(TVar 0, (TApl (TVar 2) (TVar 6)))]
                      
 