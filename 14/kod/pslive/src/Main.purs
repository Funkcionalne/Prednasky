module Main where

import Prelude

import Data.Show
import Effect (Effect)
import Effect.Console (log, logShow)

foreign import spocitajTri :: Number -> Number -> Number -> Number
foreign import efektovaFn :: Effect Unit
foreign import efektovaFn2 :: String -> Effect Unit

type Osoba = { meno :: String, vek :: Int  }
type IbaVek = { vek :: Int  }


osoba :: Osoba
osoba = { meno: "Zelda", vek: 127 }

showVek :: forall r. { vek :: Int | r } -> String
showVek { vek } = show vek


main :: Effect Unit
main = do
  log "🍝"
  log "bezim"
  logShow $ spocitajTri 1.0 2.0 3.0
  efektovaFn
  efektovaFn2 "Zelda"
  log $ showVek osoba
  log $ showVek { vek: 45, adskds: "dsasdkj" }
