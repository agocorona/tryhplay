import Haste.HPlay.View

main= runBody shtmlTest

shtmlTest :: Widget ()
shtmlTest = do
   r <- p "HTML editor"  ++> textArea "" `fire` OnKeyUp
   wprint  "Result:" 
   wraw $ rawHtml r
   
shtml= do
  id <- genNewId
  wraw $ this `innerHtml` s

rawHtml s= Perch $ \e -> innertHtml e s >> return e

innerHtml e s= ffi "(function(e,s){e.innerHtml=s})"
