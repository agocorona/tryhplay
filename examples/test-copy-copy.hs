import Haste.HPlay.View
import Haste
import Control.Monad.IO.Class
import Prelude hiding (id)

main= runBody $ do
        wbutton "ok" "ok" ! style "width:300px"
        e@(EventData typ _) <- getEventData
        wprint e


 