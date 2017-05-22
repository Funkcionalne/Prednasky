module Sheep where

data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

instance Show Sheep where
  show s = show (name s)

-- instance Monad Maybe where
--    Nothing  >>= f = Nothing
--    (Just x) >>= f = f x
--    return         = Just








maternalGrandfather :: Sheep -> Maybe Sheep

maternalGrandfather' o = if mother o == Nothing then
                            Nothing
                         else 
                            father (mother o)

maternalGrandfather s = do  m <- mother s
                            father m

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
                                  mother gm

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
                                  father gf

breedSheep :: Sheep
breedSheep =  let   adam   = Sheep "Adam" Nothing Nothing
                    eve    = Sheep "Eve" Nothing Nothing
                    uranus = Sheep "Uranus" Nothing Nothing
                    gaea   = Sheep "Gaea" Nothing Nothing
                    kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                    holly  = Sheep "Holly" (Just eve) (Just adam)
                    roger  = Sheep "Roger" (Just eve) (Just kronos)
                    molly  = Sheep "Molly" (Just holly) (Just roger)
              in Sheep "Dolly" (Just molly) Nothing

main = let dolly = breedSheep in maternalGrandfather dolly
