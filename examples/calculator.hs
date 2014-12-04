-- Minimalistic calculator 

{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}


import Haste
import Haste.HPlay.View
import Haste.HPlay.Cell
import Data.Typeable
import Prelude hiding (div,span)
import Control.Monad (when)

data Op = Op (Double -> Double -> Double) deriving Typeable

main :: IO ()
main =  do
    runBody $ div ! style "width:300px; height:300px;align:center" <<< calculator
    return()


display=  boxCell "display" :: Cell String
    
calculator= 
    mk display Nothing ! style "text-align:right;align:center;width:90%" <++ br **> 
    button '1' **> button '2' **> button '3' **> mul <++ br **> 
    button '4' **> button '5' **> button '6' **> divs <++ br **> 
    button '7' **> button '8' **> button '9' **> sum <++ br **>
    delc **> button '0' <** delLine <|>  dif <++ br **> 
    enter
          
    where
    but :: a -> String -> Widget a
    but x label=  do
        input  ! atr "type" "submit" ! atr "value" label 
               `pass` OnClick
               ! style "float:left;margin-left:8.33%;height:16.66%;width:25%"
               
        return x
    -- but :: a -> String -> Widget a
    -- but v x=  wbutton v x ! style "float:left;margin-left:8.33%;height:16.66%;width:25%"
    
    mul= but (Op (*)) "*" >>= enterOp 
    divs= but (Op (/)) "/" >>= enterOp
    sum= but (Op (+)) "+" >>= enterOp
    dif= but (Op (-)) "-" >>= enterOp
    


    enter= wbutton () "  Enter   " >> calculate
    
    button n= but n [n] >>= addNumDisplay

    enterOp op= do
        setSData True
        calculate
      **>                   -- **> forces the execution of the second setSData
                            --     even if the previous stopped
        setSData op
        
    delc= do
      but () "C"
      v <- get display
      display .= reverse . tail $ reverse v
      
    delLine= do
      but () "E"             -- if this button is pressed...
      display .= ""              -- delete the display and so on
      delSData $ Op undefined
      delSData (0 :: Double)
      delSData False
      
    calculate= do
        num'<- get display >>= return . read :: Widget Double
        num <- getSData <|> return (-1) :: Widget Double
        setSData num'
        when (num == -1) stop                 -- No previous number, do not proceed further
        Op op <-  getSData :: Widget Op
        let res= op num num'
        display .= show res
        setSData res

    addNumDisplay key= do
        clear <-  getSData <|> return False 
        case clear of
            True  -> do display .= [key] ; setSData False
            _  -> do
                v <- get display
                display .= key:v
    
    toMaybe w= (w >>= return . Just) <|> return Nothing
