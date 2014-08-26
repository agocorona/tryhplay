-- sum two numbers
-- line two

import Haste.HPlay.View
import Control.Applicative


main= runBody action

action :: Widget ()
action = do
     r  <- (+) <$> inputInt Nothing `wake` OnKeyPress <++ br
               <*> inputInt Nothing `wake` OnKeyPress <++ br
     p  (show r) ++> noWidget