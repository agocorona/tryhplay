-- show mouse events 

import Haste.HPlay.View
import Prelude hiding (div)


main= runBody mouse

mouse :: Widget ()
mouse= do
    evdata <- (div  ! style "height:100px;background-color:lightgreen;position:relative" $ h1 "Mouse events here")
                            `pass` OnMouseOut
                            `pass` OnMouseOver
                            `pass` OnMouseDown
                            `pass` OnMouseMove
                            `pass` OnMouseUp
                            `pass` OnClick
                            `pass` OnDblClick
                            `pass` OnKeyPress
                            `pass` OnKeyDown
                            `pass` OnKeyUp

    wraw $ p << ( (evName evdata) ++" "++ show (evData evdata))
