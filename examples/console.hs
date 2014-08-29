-- What a basic console application would look like using playground

import Haste.HPlay.View

main= runBody $ do
   name <- p "What is your name?" ++> getString Nothing `fire` OnKeyUp
   wraw $ p ("hello " ++ name)