module Test.Main where

import Control.Monad.Eff.JQuery
import Prelude

import Color (Color, darken)
import Color.Scheme.MaterialDesign (grey)
import DOM.HTML.Document (body)
import Data.Array ((..))
import Data.Foldable (foldMap, fold)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import FRP.Behavior.Mouse (buttons)
import FRP.Event.Mouse (up, down, move)
import Flare (numberSlider, lift, intSlider, color)
import Flare.Drawing (render)
import Flare.Drawing as D
import Graphics.Canvas as C
import Graphics.Isometric (Point, cube, filled, rotateX, rotateY, rotateZ, scale, renderScene, prism, translateX, translateY, cone)
import Graphics.Isometric.DepthSort (depthSort)
import Graphics.Isometric.Point as P
import Math (sin, cos, pi)
import Signal.DOM (animationFrame)





-- Example cube

abs num | num < 0.0 = (- num)
abs num = num
--scene2 :: Number -> Number -> D.Drawing
scene :: Number -> Number -> Number -> Number -> Number -> D.Drawing
scene xd1 yd1 rotX rotY rotZ =
  D.translate xd1 yd1 $
    renderScene { x: -1.5, y: -1.5, z: -1.5 } $
      scale 60.0 $ rotateX rotX $ rotateY rotY $ rotateZ rotZ $
           filled grey   (prism (P.point (-1.5) (-1.5) (-1.5)) 3.0 3.0 3.0)


clearCanvas ctx = do
  _<-C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

fun x y =  do
  Just canvas <- C.getCanvasElementById "canvas"
  ctx <- C.getContext2D canvas
  _<-clearCanvas ctx
  render ctx $ scene x y 10.0 10.0 10.0

{- do
  D.runFlareDrawing "controls2" "canvas2" $
    scene2 x y <$> numberSlider "RotationX" 0.0 (2.0 * pi) 0.1 0.0
           <*> numberSlider "RotationY" 0.0 (2.0 * pi) 0.1 0.0
           <*>
           <*> lift animationFrame
      -}
main = fun 500.0 270.0
