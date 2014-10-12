import Haste
import Haste.HPlay.View
import Control.Monad.IO.Class
import Debug.Trace

(!>) = flip trace

main= runBody $ test

test= do
   (submitButton  "one" `fire` OnClick  >>
    submitButton "two" `fire` OnClick)
    `wcallback` const (wraw $ p "hi")

   
