import Haste.HPlay.View
import Haste
import Control.Monad.IO.Class

main= runBody $ do
  r <- inputString Nothing `fire` OnKeyUp ! style "width:300"
  liftIO $ print "callback"
  wprint r
  
