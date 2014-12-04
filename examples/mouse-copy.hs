-- show mouse events 

import Haste.HPlay.View
import Prelude hiding (div)


main= runBody mouse

mouse :: Widget ()
mouse= do
    wraw (div  ! style "height:100px;background-color:lightgreen;position:relative" $ h1 "Mouse events here")
                            `fire` OnMouseOut
                            `fire` OnMouseOver
                            `fire` OnMouseDown
                            `fire` OnMouseOut
                            `fire` OnMouseMove
                            `fire` OnMouseUp
                            `fire` OnClick
                            `fire` OnDblClick
                            `fire` OnKeyPress
                            `fire` OnKeyDown
                            `fire` OnKeyUp
    evdata <- getEventData
    wraw $ p << ( (evName evdata) ++" "++ show (evData evdata))
