-- upravené riešenie s HaskellThonu, delitele len po sqrt
jePrvocislo n = null (delitele n) 
sqrti n = floor (sqrt $ fromIntegral n)
delitele n = [d | d <- [2..sqrti n], n `mod` d == 0] 

-- explicitne viditeľné lambda abstrakcie
jePrvocislo' =  \n -> null (delitele' n) 
sqrti' = \n -> floor (sqrt $ fromIntegral n)
delitele' = \n -> [d | d <- [2..sqrti' n], n `mod` d == 0] 

-- 'funkcionálny' snobizmus 
-- zamlčané explicitné argumenty, poučívanie kompozície fcií
jePrvocislo'' =  null . delitele''
sqrti'' = floor . sqrt . fromIntegral
delitele'' = \n -> [d | d <- [2..sqrti'' n], n `mod` d == 0] 

-- closure: sqrt a delitele sú vnorené funkcie, ktoré vidia do prostredia materskej funkcie
jePrvocislo''' n = null delitele where 
        sqrtn = floor (sqrt $ fromIntegral n)
        delitele = [d | d <- [2..sqrtn], n `mod` d == 0]

-- let vždy nahradí where, ale naopak to neplatí
jePrvocislo'''' n = let sqrtn = floor (sqrt $ fromIntegral n)
                        delitele = [d | d <- [2..sqrtn], n `mod` d == 0]
                    in     
                        null delitele

-- konečne riešenie správne aj matematicky
jePrvocislo''''' n = n>1 && null delitele where 
        sqrtn = floor (sqrt $ fromIntegral n)
        delitele = [d | d <- [2..sqrtn], n `mod` d == 0]

main = do putStrLn $ show $ filter jePrvocislo [1..100]
          putStrLn $ (show . filter jePrvocislo'') [1..100]
          putStrLn $ (show . filter jePrvocislo''') [1..100]
          putStrLn $ (show . filter jePrvocislo'''') [1..100]
          putStrLn $ (show . filter jePrvocislo''''') [1..100]
          
          
          