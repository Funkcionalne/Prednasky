module Magic11 where

keys = [[7,8,9],
        [4..6],
        [1..3]]
              
kontrapriklad :: Int
kontrapriklad = if null $ filter (\x -> x `mod` 11 > 0) obdlznikove
                then 0
                else head obdlznikove
                where obdlznikove = obdlznikoveCisla1++obdlznikoveCisla2
obdlznikoveCisla1 :: [Int]                
obdlznikoveCisla1 = [  1000*keys!!r1!!s1+100*keys!!r1!!s2+10*keys!!r2!!s2+keys!!r2!!s1 |
                        r1<-[0..2], s1<-[0..2], r2<-[0..2], s2<-[0..2]
                    ]
obdlznikoveCisla2 :: [Int]                
obdlznikoveCisla2 = [  1000*keys!!r1!!s1+100*keys!!r2!!s1+10*keys!!r2!!s2+keys!!r1!!s2 |
                        r1<-[0..2], s1<-[0..2], r2<-[0..2], s2<-[0..2]
                    ]