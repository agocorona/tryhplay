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

data OnDeviceReady= OnDeviceReady deriving Typeable

instance IsEvent OnDeviceReady (IO ()) where
   eventName= const "deviceready"
   buildHandler e cont=  do
        setIOEventData OnDeviceReady
        cont >> return()


data Op = Op (Double -> Double -> Double) deriving Typeable

main :: IO ()
main =  do
    runBody calculator
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
    wbutton () "click here"
    at "app" Insert calculator
 

display=  boxCell "display" :: Cell String
    
calculator=  
    mk display (Just "0") ! style "text-align:right" <++ br **> 
    button '1' **> button '2' **> button '3' **> mul <++ br **> 
    button '4' **> button '5' **> button '6' **> div <++ br **> 
    button '7' **> button '8' **> button '9' **> sum <++ br **>
    delc **> button '0' <** delline <|>  dif <++ br **> 
    enter
          
    where       
    updateDisplay key= do
      v <- get display
      display .= key:v 

   
    mul= wbutton (Op (*)) "*" >>= setSData 
    div= wbutton (Op (/)) "/" >>= setSData
    sum= wbutton (Op (+)) "+" >>= setSData
    dif= wbutton (Op (-)) "-" >>= setSData
    enter= wbutton () "  Enter   " >> calculate
    button n= wbutton n [n] >>= updateDisplay
    delc= do
      wbutton () "C"
      v <- get display
      display .= reverse . tail $ reverse v
    delline= do
      wbutton () "CE"
      v <- get display
      display .= "0"
    calculate= do
        num'<- get display >>= return . read :: Widget Double
        num <- getSData :: Widget Double
        setSData num'
        Op op <-  getSData
        display .= show $ op num num'
        delSData op


