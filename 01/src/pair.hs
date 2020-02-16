dvojica a b = pair
    where pair(f) = f a b

prvy p = p (\ a-> \b -> a)
    
druhy p = p (\ a-> \b -> b)
