-- This example draw a function of x between 10 and -10. You can define the function using javascript expressions

import Haste
import Haste.Graphics.Canvas
import Haste.HPlay.View
import Haste.Foreign
import Control.Applicative
import Control.Monad.IO.Class
import Data.List
import Prelude hiding(id)

main= runBody  drawcanvas

drawcanvas :: Widget ()
drawcanvas= 
 p << "This example draw a function of x between 10 and -10. You can define the function\
      \ using javascript expressions"  ++>

 (center <<< do
      let initial= "x*x+x+10;"
          sanitize str= if isInfixOf "alert" str then initial else str
      expr <- inputString (Just initial) `fire` OnKeyUp <++ br <|> return initial
      wraw $ canvas ! id "canvas"  $ noHtml
      wraw $ draw $sanitize expr)


  where

  evalFormula :: String  -> IO Double
  evalFormula= ffi $ toJSString "(function(exp){ return eval(exp);})"

  draw expr= liftIO $ do
    Just can <- getCanvasById "canvas"
    let range= [-10..10]
        exprs = Prelude.map (\v -> subst 'x' (show v) expr) range
    ps <- mapM evalFormula  exprs
    render can $ scale (3,1) $ translate (50,130) $ rotate pi $ stroke $do
        line (-10,0) (10,0)
        line (0,30) (0,-30)
        path $ zip range ps

  subst _ _ []= []
  subst c v (c':cs)
    | c==c' =  v++ subst c v cs
    | otherwise= c':subst c v cs
