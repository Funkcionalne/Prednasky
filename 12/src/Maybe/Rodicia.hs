module Rodicia where

import Data.Maybe
import Control.Monad
----------
mama :: String -> Maybe String
mama "Anna" = Just "Eva"
mama "Betka" = Just "Hanka"
mama "Danka" = Just "Iveta"
mama "Eva" = Just "Renata"
mama "Hanka" = Just "Michaela"
mama "Iveta" = Just "Svetlana"
mama "Adam" = Just "Danka"
mama "Boris" = Just "Eva"
mama "Dusan" = Just "Hanka"
mama "Emil" = Just "Iveta"
mama "Gusto" = Just "Maria"
mama "Ivan" = Just "Renata"
mama "Maria" = Just "Elena"
mama _ = Nothing
-----------
otec :: String -> Maybe String
otec "Anna" = Just "Dusan"
otec "Betka" = Just "Emil"
otec "Danka" = Just "Gusto"
otec "Eva" = Just "Ivan"
otec "Hanka" = Just "Martin"
otec "Iveta" = Just "Peter"
otec "Adam" = Just "Emil"
otec "Boris" = Just "Gusto"
otec "Dusan" = Just "Ivan"
otec "Emil" = Just "Peter"
otec "Gusto" = Just "Rado"
otec "Ivan" = Just "Zigmund"
otec _ = Nothing

-- Vrati to iste, co mama, iba zacne prefixom "Mila "
mila_mama' :: String -> Maybe String
-- mila_mama _ = Nothing
-- klasicky:
mila_mama' x = if mama x == Nothing then Nothing else Just ("Mila " ++ (fromJust (mama x)))
-- monadicky:
mila_mama :: String -> Maybe String
mila_mama x =   do  m <- mama x
                    return ("Mila " ++ m)

-- Vrati otcovho otca
praotec :: String -> Maybe String
praotec x = do o <- otec x
               po <- otec o
               return po

{-
otec "Anna" = Just "Dusan"
praotec "Anna" = Just "Ivan"

otec "Iveta" =  Just "Peter"
praotec "Iveta" = Nothing
-}

-- Pre k=1 mama, pre k=2 babka, pre k=3 prababka ...
k_mama :: Int -> String -> Maybe String
k_mama 1 x = mama x
k_mama k x = do m <- mama x
                mk <- k_mama (k-1) m
                return mk

-- Vrati rodicov
rodicia' :: String -> [String]
rodicia' x = (if otec x == Nothing then [] else [fromJust (otec x)])
             ++
             (if mama x == Nothing then [] else [fromJust (mama x)])

rodicia :: String -> Maybe [String]
-- rodicia x = sequence [otec x, mama x]

-- rodicia x = do { o<-otec x; return [o] } `mplus` (do m<-mama x; return [m])
-- rodicia "Iveta" = Just ["Peter"]

-- rodicia x = do { o<-otec x; m<-mama x; return ([o] `mplus` [m]) }
-- rodicia "Iveta" = Just ["Peter","Svetlana"]

-- rodicia "Maria" = Just ["Elena"]

 
-- Vrati prarodicov
prarodicia :: String -> [String]
prarodicia _ = []

-- Vrati rodicov v k-tej generacii
k_rodicia :: Int ->String -> [String]
k_rodicia k _ = []

-- Vrati vsetkych znamych predkov
predkovia :: String -> [String]
predkovia _ = []
