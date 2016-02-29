module RozdielSuctu where

rozdelParneNeparne :: [Integer] -> ([Integer],[Integer])
rozdelParneNeparne [] = ([],[])
rozdelParneNeparne (x:xs) = (xp, x:xn) where (xp, xn) = rozdelNeparneParne xs

rozdelNeparneParne :: [Integer] -> ([Integer],[Integer])
rozdelNeparneParne [] = ([],[])
rozdelNeparneParne (x:xs) = (x:xp, xn) where  (xp, xn) = rozdelParneNeparne xs

rozdielSuctu :: [Integer] -> Integer
rozdielSuctu xs = sum parneMiesta - sum neparneMiesta
                where (parneMiesta, neparneMiesta) = rozdelParneNeparne xs

-- Celozrnné riešenie:
rozdielSuctu' :: [Integer] -> Integer
rozdielSuctu'  = negate . foldr (-) 0
  -- alebo len -foldr(-)0

-- kompromis 1 - zbierame parne a neparne prvky do zoznamov
rozdielSuctu'' :: [Integer] -> Integer
rozdielSuctu'' xs  = (sum p) - (sum n)
            where (p,n) = foldr (\x -> \(p,n) -> (n,x:p)) ([],[]) xs

-- kompromis 2 - preco nepocitat sucet uz hned
rozdielSuctu''' :: [Integer] -> Integer
rozdielSuctu''' xs  = p - n
            where (p,n) = foldr (\x -> \(p,n) -> (n,p+x)) (0,0) xs

-- kompromis 3 - 
rozdielSuctu'''' :: [Integer] -> Integer
rozdielSuctu'''' xs  = uncurry (-) $ foldr (\x -> \(p,n) -> (n,p+x)) (0,0) xs

-- kompromis 4 - 
rozdielSuctu''''' :: [Integer] -> Integer
rozdielSuctu'''''   = uncurry (-) . foldr (\x -> \(p,n) -> (n,p+x)) (0,0)

-- "?: " :type uncurry
-- uncurry :: (a -> b -> c) -> (a, b) -> c 