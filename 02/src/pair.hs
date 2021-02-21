dvojica a b = pair
    where pair(f) = f a b

-- inak
dvojica'  a b   = \f -> f a b
dvojica'' a b f = f a b

main = do putStrLn $ show [ prvy (dvojica 4 5), druhy (dvojica 4 5) ]



prvy p = p (\ a-> \b -> a)
druhy p = p (\ a-> \b -> b)

prvy' p = p true
	where true a b = a   
druhy' p = p false
	where false a b = b

