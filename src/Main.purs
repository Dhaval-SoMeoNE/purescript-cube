module Main where

import Color.Scheme.MaterialDesign (grey)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (body, getPageX, getPageY, on)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import DOM (DOM)
import Data.Foldable (foldMap, fold)
import Data.Foreign (readString)
import Data.Int (fromNumber)
import Data.Maybe (fromJust, fromMaybe)
import Flare.Drawing (Drawing, render)
import Flare.Drawing as D
import Graphics.Canvas (CANVAS, Context2D)
import Graphics.Canvas as C
import Graphics.Isometric (Point, cube, filled, rotateX, rotateY, rotateZ, scale, renderScene, prism, translateX, translateY, cone)
import Graphics.Isometric.DepthSort (depthSort)
import Graphics.Isometric.Point as P
import Math (cos, pi, sin, trunc)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, negate, pure, show, unit, void, ($), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), (||))
import Signal.DOM (animationFrame)
import Signal.Time (now)


foreign import getSpeed ::  Number -> Number
foreign import changeSpeed :: Number-> Number
foreign import flag :: Boolean -> Boolean

startMouseHandlers :: forall t149.            
  Eff                   
    ( dom :: DOM      
    , canvas :: CANVAS  
    , console :: CONSOLE
    , timer :: TIMER    
    | t149              
    )                   
    Unit

startMouseHandlers = do
  body <- body
  let downHandler event jq = do
        downX <- getPageX event
        downY <- getPageY event
        timeDown <- now
        let upHandler event' jq' = do
              upX <- getPageX event'
              upY <- getPageY event'
              timeUp <- now
              let dx = upX - downX
              let temp = (((downX - upX )/(timeDown - timeUp)) * 500.0) 
              let speed = if((getSpeed temp) == 0.0)
                             then changeSpeed (temp) 
                             else changeSpeed (0.0)
              rotateCube 0.0
        on "mouseup" upHandler body
  on "mousedown" downHandler body

-- Example cube

abs :: Int -> Int
abs num | num < 0 = (- num)
abs num = num


scene ::Number -> Drawing
scene rotX =
  D.translate 550.0 300.0 $
    renderScene { x: -1.5, y: -3.5, z: 3.5 } $
      scale 80.0 $ rotateZ rotX $
           filled grey   (prism (P.point (-1.5) (-1.5) (-1.5)) 3.0 3.0 3.0)


clearCanvas :: forall t22.              
  Context2D              
  -> Eff                 
       ( canvas :: CANVAS
       | t22             
       )                 
       Context2D
clearCanvas ctx = do
  _<-C.setFillStyle "#FFFFFF" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

fun :: forall t27.              
  Number                 
  -> Eff                 
       ( canvas :: CANVAS
       | t27             
       )                 
       Unit
       
fun rotX =  do
  mcanvas <- C.getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- C.getContext2D canvas
  _<-clearCanvas ctx
  render ctx $ scene rotX

rotateCube :: forall t58.              
  Number                 
  -> Eff                 
       ( canvas :: CANVAS
       , timer :: TIMER  
       | t58             
       )                 
       Unit
rotateCube angle= do
     fun angle
     let newSpeed =  (getSpeed angle)
     if ((newSpeed <20.0) )
         then pure unit
         else let timep = (20000/(abs (fromMaybe 200 (fromNumber (trunc (newSpeed)))))) 
         in void $ setTimeout timep (rotateCube (angle + 0.1))
               
main :: forall t141.            
  Eff                   
    ( canvas :: CANVAS  
    , console :: CONSOLE
    , dom :: DOM        
    , timer :: TIMER    
    | t141              
    )                   
    Unit
main = do
   fun 0.0
   startMouseHandlers