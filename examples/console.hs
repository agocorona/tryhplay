-- What a basic console application would look like using hplayground

import Haste.HPlay.View
import Prelude hiding(print)

main= runBody $ do
   print "What is your name?"
   name <- getString Nothing `fire` OnKeyUp
   print $ "hello " ++ name ++"!"
   
print= wraw . pre