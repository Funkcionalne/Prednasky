module Papalasi where
import Data.String
import Data.Map
import Data.Maybe
import Test.QuickCheck
import Text.Show.Functions

parse :: String -> Maybe Osoba
parse input = if length split /= 3 || rc `mod` 11 /= 0 then Nothing
              else Just $ osoba(krstne, priezvisko, rc)
              where split = words input
                    [krstne, priezvisko, rcStr] = split
                    rc = read rcStr :: Int
                
data Osoba = Osoba { krstne :: String, priezvisko :: String, rc :: Int } deriving(Show)

osoba :: (String, String, Int) -> Osoba
--osoba (krstne, priezvisko, rc) = Osoba krstne priezvisko rc
osoba = uncurry3 Osoba
        where uncurry3 f (a,b,c) = f a b c

data Papalasi a = Papalasi {
  premier :: a,
  vlada :: Map String a,
  parlament :: [a]
}  deriving(Show, Eq)

instance Functor Papalasi where
  fmap f paps = Papalasi {
    premier = f (premier paps),
    --premier = f (fromJust (Data.Map.lookup "financ" (vlada paps))),
    vlada = f <$> vlada paps,
    --parlament = f <$> parlament paps
    parlament = fmap f (parlament paps)
  }

inp' :: Papalasi String
inp' = Papalasi
  ("Juraj H 121")
  (Data.Map.fromList 
    [ ("financ", ("Igor M 55"))
    , ("defenc", ("Roman  M 66"))
    , ("justice", ("Maria K 44"))
    ])
  ([("Robert F 38"), ("Peter P 33")])

-- parse <$> inp'

inp :: Papalasi (String, String, Int)
inp = Papalasi 
  ("Juraj", "H", 121)
  (Data.Map.fromList 
    [ ("financ", ("Igor", "M", 55))
    , ("defenc", ("Roman", "M", 66))
    , ("justice", ("Maria", "K", 44))
    ])
  ([("Robert", "F", 38), ("Peter", "P", 33)])

-- osoba <$> inp  

instance Arbitrary a => Arbitrary (Papalasi a) where
  arbitrary = do
    r_premier <- arbitrary
    r_vlada <- arbitrary
    r_parlament <- arbitrary
    return $ Papalasi
      { premier = r_premier
      , vlada = r_vlada
      , parlament = r_parlament
      }

main :: IO ()
main  = do functorCheck1 
           functorCheck2

functorCheck1 = quickCheck((\paps -> fmap id paps == paps)::Papalasi String -> Bool)

functorCheck2 = quickCheck((\paps -> \p -> \q -> fmap (p.q) paps == ((fmap p).(fmap q)) paps)::Papalasi String -> (String->String)-> (String->String) -> Bool)
