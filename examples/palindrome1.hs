-- To search palindromes: one box present the other's reversed. It is also an 
-- example of cell usage

import Haste.HPlay.View
import Haste.HPlay.Cell as Cell
import Control.Applicative

main= runBody $ do
 wraw $ p << "To search palindromes: one box present the other's reversed. It is also\
                 \ an example of cell usage" 
 let entry= boxCell "entry" :: Cell String
     revEntry= boxCell "revEntry"

 r <- center <<< ((mk entry    Nothing `fire` OnKeyUp >>= return . Left) <++ br <|>
                  (mk revEntry Nothing `fire` OnKeyUp >>= return . Right))

 case r of
  Left s  -> do
   r  <- Cell.get entry >>= return . reverse
   revEntry .= r
  Right s -> do
   r' <- Cell.get revEntry >>= return . reverse
   entry .= r'

