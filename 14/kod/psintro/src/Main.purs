module Main where

import Prelude
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.JSDate (now, getTime)
import Data.Maybe (fromJust)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas
  ( CanvasElement
  , Context2D
  , Dimensions
  , fillRect
  , getCanvasElementById
  , getCanvasDimensions
  , getContext2D
  , setCanvasHeight
  , setCanvasWidth
  , setFillStyle
  )
import Math (cos, sin)
import Partial.Unsafe (unsafePartial)
import Web.HTML (Window, window)
import Web.HTML.Window
  ( RequestAnimationFrameId
  , innerHeight
  , innerWidth
  , requestAnimationFrame
  )

type Env
  = { canvas :: CanvasElement
    , context :: Context2D
    , window :: Window
    , positions :: Array Position
    }

type Position
  = { x :: Number, y :: Number }

bipolarToPositive :: Number -> Number
bipolarToPositive x = (1.0 + x) / 2.0

msToS :: Number -> Number
msToS = (_ / 1000.0)

randomPosition :: Dimensions -> Effect Position
randomPosition { width, height } = do
  x <- random
  y <- random
  pure { x: x * width, y: y * height }

renderStar :: Env -> Number -> Position -> Effect Unit
renderStar { context } currentTime position =
  fillRect context
    { x: nx
    , y: ny
    , width: s
    , height: s
    }
  where
  nx = position.x + (sin ((currentTime + position.y) * s / 10.0)) * s * 10.0

  ny = position.y + (cos ((currentTime + position.x) * s / 10.0)) * s * 5.0

  s = 1.0 + (bipolarToPositive (sin (position.x * position.y * 1337.0))) * 5.0

render :: Env -> Effect RequestAnimationFrameId
render env@{ canvas, window, context, positions } =
  requestAnimationFrame
    ( do
        _ <- render env
        currentTime <- (getTime >>> msToS) <$> now
        { width, height } <- getCanvasDimensions canvas
        setFillStyle context "#577590"
        fillRect context { x: 0.0, y: 0.0, width, height }
        setFillStyle context "white"
        sequence_ $ renderStar env currentTime <$> positions
    )
    window

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "cnvs"
  let
    canvas = unsafePartial $ fromJust maybeCanvas
  wnd <- window
  innerWidth wnd >>= setCanvasWidth canvas <<< toNumber
  innerHeight wnd >>= setCanvasHeight canvas <<< toNumber
  ctx <- getContext2D canvas
  cd <- getCanvasDimensions canvas
  positions <- replicateA 100 $ randomPosition cd
  void
    $ render
        { canvas: canvas
        , context: ctx
        , window: wnd
        , positions: positions
        }
