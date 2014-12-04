{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
-- Hello-World of the cordova/phonegap application using Haskell.
-- So that Haste-Haskell can be used to create hybrid smartphone applications
-- The original JavaScript hello world is installed following the instructions of this page
--
-- http://cordova.apache.org/docs/en/4.0.0/guide_cli_index.md.html#The%20Command-Line%20Interface
--
-- follow the steps to install the hello world app.
--
-- install the browser platform, the simplest one:
--
-- $ cordova platform  add browser
--
-- $ cordova build
--
-- $ cordova run
--
-- install Haste and the last version of the Perch library
-- 
-- cabal install haste
-- 
-- git clone https://github.com/agocorona/haste-perch
--
-- compile this program
--
-- hastec cordova.hs
--
-- gt the JavaScript file and substitute the index.js of the installed hello-world application
--
-- rebuild
--
-- cordova build
--
-- cordova run
--

import Haste
import Haste.HPlay.View
import Haste.HPlay.Cell
import Data.Typeable
import Control.Monad
import Prelude hiding(id,div)

data OnDeviceReady= OnDeviceReady deriving Typeable

instance IsEvent OnDeviceReady (IO ()) where
   eventName= const "deviceready"
   buildHandler e cont=  do
        setIOEventData OnDeviceReady
        cont >> return()


data Op = Op (Double -> Double -> Double) deriving Typeable

main :: IO ()
main =  do
    runBody cor
    return()

cor= do
    wraw this `fire`  OnDeviceReady    
    wraw $ do
        forElems (".listening") $ this ! style "display:none"
        forElems (".received")  $ do
            this ! style "display:block"
            br
            i "RUNNING a HASKELL program"
            br
            i "compiled with the HASTE compiler"
            br
            i "and hplayground"
            div ! id "button" $ noHtml
            
    at "button" Insert $ wbutton () "click here"
    at ".app" Insert calculator
 

display=  boxCell "display" :: Cell String
    
calculator= table <<<(
    (tr <<< td ! atr "colspan" "4" <<< mk display Nothing)       **>
    (tr <<< (button '1' <|> button '2' <|> button '3' <|> mul))  <|> 
    (tr <<< (button '4' <|> button '5' <|> button '6' <|> divs)) <|> 
    (tr <<< (button '7' <|> button '8' <|> button '9' <|> sum))  <|>
    (tr <<< (delc       <|> button '0' <|> delLine    <|> dif))  <|> 
    (tr <<< enter))
          
    where
    but :: a -> String -> Widget a
    but x l= wbutton x l ! style "height:20px;width:100%"
               
    mul=  td <<< but (Op (*)) "*" >>= enterOp 
    divs= td <<< but (Op (/)) "/" >>= enterOp
    sum=  td <<< but (Op (+)) "+" >>= enterOp
    dif=  td <<< but (Op (-)) "-" >>= enterOp
    


    enter= td ! atr "colspan" "4" <<< do
                wbutton () "Enter"  ! style "width:100%"
                calculate
    
    button n= td <<< (but n [n] >>= addNumDisplay)

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
                display .= v++ [key]
    
    toMaybe w= (w >>= return . Just) <|> return Nothing
