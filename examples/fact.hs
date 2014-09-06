-- factorial 

import Haste.HPlay.View

main = runBody $ do
-- wraw: display raw HTML and return ()
   wraw $ p $ "enter the number"
   num <- inputInteger Nothing `fire` OnKeyUp
   wraw $ p $  show (fact num)


fact 0= 1
fact n= n * fact (n-1)

