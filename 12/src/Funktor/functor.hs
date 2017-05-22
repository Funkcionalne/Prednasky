{-
instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)
-}

-- Functors abstract the idea of mapping a function over each element of a structure

-- vyhodnotte
e1 = fmap (+1) Nothing
e2 = fmap (*2) (Just 3)
e3 = fmap not (Just False)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving(Show)

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- vyhodnotte  
e4 = fmap length (Leaf "abc")
e5 = fmap even (Node (Leaf 1) (Leaf 2))

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

e6 = inc (Just 1)
e7 = inc [1,2,3,4,5]
e8 = inc (Node (Leaf 1) (Leaf 2))
