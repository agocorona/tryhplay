-- factorial 

import Haste.HPlay.View
import Prelude hiding (print)

main = runBody $ do
   print $ "enter the number"
   num <- inputInteger Nothing `fire` OnKeyUp
   print (fact num :: Integer)


fact 0= 1
fact n= n * fact (n-1)

-- wraw: display raw HTML and return ()
-- pre is the HTML tag 
print :: ToElem a => a -> Widget ()
print= wraw . pre