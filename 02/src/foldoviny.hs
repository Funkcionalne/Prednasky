module Foldoviny where

powerSet  :: [t] -> [[t]]
powerSet []     = [[]]
powerSet (x:xs) = [ x:p | p <- ps ] ++ ps
                  where ps = powerSet xs
                  
powerSet'  :: [t] -> [[t]]
powerSet' xs = foldr (\x -> \ps -> map (x:) ps ++ ps) [[]] xs

powerSet''  :: [t] -> [[t]]
powerSet''  = foldr (\x -> \ps -> map (x:) ps ++ ps) [[]]

powerSet'''  :: [t] -> [[t]]
powerSet'''  = foldr pom [[]] where    
          pom x ps = map (x:) ps ++ ps

-- naprogramujme take pomocou foldl/r          
take' :: Int -> [a] -> [a]
take' n xs  =  (foldr pom (\_ -> []) xs) n where  
                pom x h = \n -> if n == 0 then []
                                 else x:(h (n-1))

take'' :: Int -> [a] -> [a]
take'' n xs  =  (foldr pom (\_ -> []) xs) n where  
                  pom x h n = if n == 0 then []
                              else x:(h (n-1))
                              
take''' n xs = foldr (\a h -> \n -> case n of
              0 -> []
              n -> a:(h (n-1)) )
			  (\_ -> [])
		   	xs
		   	n
                      
-- poslednych 5                       
take_ :: Int -> [a] -> [a]
take_ n xs  =  snd $ foldr pom (n, []) xs where  
                pom x (n, vysl) = if n <= 0 then (n, vysl)
                                  else (n-1, x:vysl)

-- prvych 5, ale reversnute, preto reverse                       
take__ :: Int -> [a] -> [a]
take__ n xs  =  reverse $ snd $ foldl pom (n, []) xs where  
                pom (n, vysl) x = if n <= 0 then (n, vysl)
                                  else (n-1, x:vysl)
                                  