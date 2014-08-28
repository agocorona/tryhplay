-- sum two numbers
-- line two

import Haste.HPlay.View
import Control.Applicative


main= runBody sumTwo

sumTwo :: Widget ()
sumTwo = p  "This widget sum two numbers and append the result. Using applicative and monadic expressions" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number"  ++> br
                   ++> inputInt Nothing `fire` OnKeyUp  <++ br
              <*> fromStr "second number " ++> br
                   ++> (inputInt Nothing `validate` less3 `fire` OnKeyUp )  <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> return())

  where
  less3 x= if x < 3 then return Nothing else  return . Just $ b " no more than 2 please"
