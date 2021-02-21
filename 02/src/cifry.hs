import Test.QuickCheck

pocetCifier :: Integer -> Int
--pocetCifier n = length $ show n
pocetCifier = length . show

pocetCifier' :: Integer -> Int
--pocetCifier' n =  fromIntegral $ ceiling $ (logBase 10 (fromIntegral n))
pocetCifier' =  fromIntegral . ceiling . (logBase 10) . fromIntegral


pocetCifier'' :: Integer -> Int
--pocetCifier'' n =  length $ takeWhile (/=0) $ iterate (`div` 10) n
pocetCifier'' =  length . takeWhile (/=0) . iterate (`div` 10)

hypoteza1 = quickCheck(\n -> (n > 0) ==> pocetCifier n == pocetCifier'' n)
hypoteza2 = quickCheck(\n -> (n > 0) ==> pocetCifier n == pocetCifier' n)
hypoteza2' = quickCheck(\n -> (n > 1) ==> pocetCifier n == pocetCifier' n)
hypoteza2'' = quickCheck(\n -> (n > 10) ==> pocetCifier n == pocetCifier' n)


-- plati/neplati ?

