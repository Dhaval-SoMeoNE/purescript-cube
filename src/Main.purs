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
import Math as Math
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, discard, negate, pure, show, unit, void, ($), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), (||))
import Signal.DOM (animationFrame)
import Signal.Time (now)

foreign import getSpeed ::  Number -> Number
foreign import changeSpeed :: Number-> Number
foreign import getAxis :: Char -> Char
foreign import changeAxis :: Char -> Char
foreign import changeDirection :: Number -> Number
foreign import getDirection :: Number -> Number

startMouseHandlers :: forall t114.            
  Eff                   
    ( dom :: DOM        
    , console :: CONSOLE
    , timer :: TIMER    
    | t114              
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
              let dy = upY - downY
              let moddx = abs1 dx
              let moddy = abs1 dy 
              let axis 
                      | moddx > moddy = (changeAxis 'x')
                      | true    = (changeAxis 'y')
              let directon 
                    | axis=='x' = changeDirection dx
                    | true      = changeDirection dy
              let temp = (((Math.max moddx moddy)/(timeUp-timeDown)) * 500.0) 
              log (show (changeSpeed (temp))<> show "change speed")
        on "mouseup" upHandler body
  on "mousedown" downHandler body

-- Example cube
abs1 ::Number -> Number
abs1 num | num < 0.0 = (- num)
abs1 num = num

abs :: Int -> Int
abs num | num < 0 = (- num)
abs num = num


scene ::Number->Char -> Drawing

scene rotAngle 'y'=
  D.translate 550.0 300.0 $
    renderScene { x: -1.5, y: -3.5, z: 3.5 } $
      scale 80.0 $ rotateY rotAngle $
           filled grey (prism (P.point (-1.5) (-1.5) (-1.5)) 3.0 3.0 3.0)


scene rotAngle axis=
  D.translate 550.0 300.0 $
    renderScene { x: -1.5, y: -3.5, z: 3.5 } $
      scale 80.0 $ rotateZ rotAngle $
           filled grey (prism (P.point (-1.5) (-1.5) (-1.5)) 3.0 3.0 3.0)


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

       
fun :: forall t57.                
  Number                   
  -> Eff                   
       ( canvas :: CANVAS  
       , console :: CONSOLE
       | t57               
       )                   
       Unit
fun rotX =  do
  mcanvas <- C.getCanvasElementById "canvas"
  let canvas = unsafePartial (fromJust mcanvas)
  ctx <- C.getContext2D canvas
  _<-clearCanvas ctx
  let axis = getAxis 'r'
  render ctx $ (scene rotX axis)

rotateCube :: forall t140.               
  Number                   
  -> Eff                   
       ( canvas :: CANVAS  
       , console :: CONSOLE
       , timer :: TIMER    
       | t140              
       )                   
       Unit
rotateCube angle= do
     fun angle
     let newSpeed =  (getSpeed angle)
     let direction = getDirection 1.0
     if ((newSpeed <20.0) )
         then void $ setTimeout 500 (rotateCube (angle + (0.0*direction)))
         else let timep = (10000/(abs (fromMaybe 200 (fromNumber (Math.trunc (newSpeed)))))) 
         in void $ setTimeout timep (rotateCube (angle + (0.1*direction)))
               
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
   rotateCube 0.0
   startMouseHandlers
