-- To search palindromes: one box present the other's reversed. It is also an example of cell usage

import Haste.HPlay.View
import Haste.HPlay.Cell as Cell
import Control.Applicative

main= runBody $ do
 wraw $ p << "To search palindromes: one box present the other's reversed. It is also\
                 \ an example of cell usage" 
 
 let entry   = boxCell "entry" :: Cell String
     revEntry= boxCell "revEntry"

 r <- center <<< ((mk entry    Nothing ! atr "size" "10" `fire` OnKeyUp >>= return . Left) <++ br <|>
                  (mk revEntry Nothing ! atr "size" "10" `fire` OnKeyUp >>= return . Right))

 -- a cell can be read anywere in the code with  Cell.get 
 -- In this case the value is read directly from the expression.
 
 case r of
    Left s  -> do
        revEntry .= reverse s   
    Right s -> do
        entry .= reverse s

