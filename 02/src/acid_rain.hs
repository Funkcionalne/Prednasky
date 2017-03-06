-- foldr f z . g (:) [] = g f z

{--
g     :: (Int ->[Int] -> [Int]) -> [Int] -> Int -> [Int]
g h w 0 = []
g h w n = h n (g h w (n-1))
--}

{--
g     :: (Int ->[t] -> [t]) -> [Int] -> Int -> [t]
g h w 0 = []
g h w n = h n (g h w (n-1))
--}

g     :: (Integer ->u -> u) -> u -> Integer -> u
g h w 0 = w
g h w n = h n (g h w (n-1))


-- ((foldr (*) 1) . (g (:) [])) 100
-- g (*) 1 100

{--

--}
