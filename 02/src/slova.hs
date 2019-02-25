module Slova where
import Data.List

slova :: Int -> [String]
slova 0	= [[]]
slova k = [ ch:w | w <-slova (k-1), ch <- "ABCDEF"]

--length $ slova 3 = 216

slova' :: Int -> [String]
slova' 0	= [[]]
slova' k = slova' (k-1) ++ [ ch:w | w <-slova' (k-1), ch <- "ABCDEF"]
--length $ slova' 2 = 49 != 1+6+36 = 43
--slova' 2 = ["","A","B","C","D","E","F","A","B","C","D","E","F","AA","BA","CA","DA","EA","FA","AB","BB","CB","DB","EB","FB","AC","BC","CC","DC","EC","FC","AD","BD","CD","DD","ED","FD","AE","BE","CE","DE","EE","FE","AF","BF","CF","DF","EF","FF"]

--length $ nub $ slova' 2 = 43

--BTW. kolko je 1+6+36+...+6^k (slova dlzky najviac k) ?

explicit = map (\k->(6^(k+1)-1) `div` 5) [1..5]

slova'' :: Int -> [String]
slova'' 0	= [[]]
slova'' k = ws ++ [ ch:w | w <-ws, ch <- "ABCDEF"] 
            where ws = slova'' (k-1)
--length $ nub $ slova'' 2 = 43

slova''' :: Int -> [String]
slova''' 0	= [[]]
slova''' k = let ws = slova''' (k-1) in
                ws ++ [ ch:w | w <-ws, ch <- "ABCDEF"] 
--length $ nub $ slova''' 2 = 43
