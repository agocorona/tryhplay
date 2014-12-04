-- sum two numbers

import Haste.HPlay.View
import Control.Applicative


main= runBody sumTwo

sumTwo :: Widget ()
sumTwo = p  "This widget sum two numbers and append the result. Using applicative and monadic expressions" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number"  ++> br
                   ++> inputInt Nothing `fire` OnKeyUp ! atr "size" "7"  <++ br
              <*> fromStr "second number " ++> br
                   ++> (inputInt Nothing   ! atr "size" "7" `validate` less10 `fire` OnKeyUp )  <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> return())

  where
  less10 x= if x < 10 then return Nothing else  return . Just $ b " no more than 10 please"
