import Haste.HPlay.View
import Haste.DOM

main= runBody shtmlTest

shtmlTest :: Widget ()
shtmlTest = do
   r <- textArea "" ! atr "placeholder" "HTML editor" `fire` OnKeyUp
   wprint r
   wraw $ rawHtml r
   


rawHtml s= Perch $ \e -> setProp e "innerHTML" s >> return e

