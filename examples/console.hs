-- What a basic console application would look like using hplayground

import Haste.HPlay.View
import Prelude hiding(print)

main= runBody $ do
   name <- p "What is your name?" ++> getString Nothing `fire` OnKeyUp
   print $ "hello " ++ name ++"!"
   
print= wraw . pre