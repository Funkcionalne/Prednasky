module Sheep where
import Data.Maybe
import Control.Monad

-- a sheep has its name, and maybe mother and father
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}   deriving (Eq)

instance Show Sheep where
  show s = show (name s)

-- toto je priklad na Maybe monadu, ktora je definovana takto
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing    -- `bind`
--    (Just x) >>= f = f x
--    return         = Just       -- return

------------------------------------------------------------------------------------

maternalGrandfather :: Sheep -> Maybe Sheep

-- klasicky: stary otec z matkinej strany
maternalGrandfather' o = if mother o == Nothing then
                            Nothing
                         else 
                            father (fromJust (mother o))

-- monadicky: stary otec z matkinej strany
maternalGrandfather s = do  m <- mother s
                            father m

-- otca matky matka
fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
                                  mother gm

-- matky otca otec
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
                                  father gf

parents_ :: Sheep -> [Maybe Sheep]
parents_ x = [father x, mother x]
-- parents_ dolly = [Nothing,Just "Molly"]

parents :: Sheep -> Maybe [Sheep]
parents x = sequence [father x, mother x]
-- parents dolly = Nothing

parents' :: Sheep -> [Sheep]
parents' x = (if father x == Nothing then [] else [fromJust (father x)])
             ++
             (if mother x == Nothing then [] else [fromJust (mother x)])
-- parents' dolly = ["Molly"]

parents'' :: Sheep -> Maybe [Sheep]
parents'' x = do { o<-father x; return [o] } `mplus` (do m<-mother x; return [m])
-- parents'' dolly = Just ["Molly"]

parents''' :: Sheep -> Maybe [Sheep]
parents''' x = do { o<-father x; m<-mother x; return ([o] `mplus` [m]) }
-- parents''' dolly = Nothing

---- nejake data:
adam   = Sheep "Adam"    Nothing Nothing
eve    = Sheep "Eve"     Nothing Nothing
uranus = Sheep "Uranus"  Nothing Nothing
gaea   = Sheep "Gaea"    Nothing Nothing
kronos = Sheep "Kronos"  (Just gaea) (Just uranus)
holly  = Sheep "Holly"   (Just eve) (Just adam)
roger  = Sheep "Roger"   (Just eve) (Just kronos)
molly  = Sheep "Molly"   (Just holly) (Just roger)
dolly  = Sheep "Dolly"   (Just molly) Nothing

--maternalGrandfather dolly
-- mothersPaternalGrandfather dolly
-- maternalGrandfather' dolly
-- parents dolly
-- parents' dolly
-- parents'' dolly
--parents''' dolly
main = parents_ dolly






-------------------------------
k_mother :: Int -> Sheep -> Maybe Sheep
k_mother 0 x = Just x
k_mother 1 x = mother x
k_mother k x = do m <- mother x
                  mk <- k_mother (k-1) m
                  return mk

k_predecesors :: Int -> Sheep -> [Sheep]
k_predecesors 1 x = parents' x
k_predecesors k x = do pred <- k_predecesors (k-1) x
                       mk <- parents' pred
                       return mk
