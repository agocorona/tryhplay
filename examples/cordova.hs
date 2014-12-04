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
import Haste.Perch

main :: IO ()
main = do
  doc <- getDocument
  listen doc  "deviceready"   doit 
  body <- getBody
  (flip build) body $ b "cordova/phonegap hello-world demo. Read the instructions in the source code"
  return ()
     

doit ::  IO ()
doit  = do
 forElems' (".listening") $ this ! style "display:none"
 forElems' (".received")  $ do
      this ! style "display:block"
      br
      i "RUNNIN a HASKELL program"
      br
      i "compiled with the HASTE compiler"
      

