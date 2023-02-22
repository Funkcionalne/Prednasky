module Iteruj where -- exportujeme vsetko
--module Iteruj(iteruj, iteruj', iteruj'', iteruj''', iteruj'''', 
--    iteruj_foldr, iteruj_foldl, iteruj_using_iterate, iteruj_funkciu) where 

iterujf 	:: Int -> ((Float -> Float) -> (Float -> Float) )
iterujf  = iteruj

iteruj 	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj 0	f  = (\x -> x)			-- identita
iteruj n	f  = (\x -> f (iteruj (n-1) f x))

iteruj' 	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj' 0	f  = (\x -> x)			-- identita
iteruj' n	f  = (\x -> iteruj' (n-1) f (f x))

iteruj'' 	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj'' 0	f x = x
iteruj'' n	f x = f (iteruj'' (n-1) f x)

iteruj''' 	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj''' 0	f  = id
iteruj''' n	f = f  . (iteruj''' (n-1) f)

iteruj'''' 	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj'''' 0	f  = id
iteruj'''' n	f = (iteruj'''' (n-1) f) . f 

iteruj_foldr 	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj_foldr n f	= foldr (.) id (replicate n f)

iteruj_foldl 	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj_foldl n f	= foldl (.) id (take n (cycle [f]))

iteruj_using_iterate	:: Int -> ((t -> t) -> (t -> t) ) 
iteruj_using_iterate n f x	= iterate f x !! n

iteruj_funkciu :: Int -> ((t -> t) -> (t->t))
iteruj_funkciu n f  = iteruj_f !! n
    where iteruj_f = id:[f . g | g <- iteruj_f]
