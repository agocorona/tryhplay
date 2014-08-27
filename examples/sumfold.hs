-- Sum three numbers and append the result using a fold

import Haste.HPlay.View
import Data.Monoid

main= runBody $ sumfold 3

sumfold :: Int -> Widget ()
sumfold n =  p  ("This widget sum "++ show n ++" numbers and append the result using a fold") ++>
       (p <<< do
         r <- foldl (<>)  (return 0) . take n $ repeat $ inputInt Nothing `fire` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ())

instance Monoid Int where
  mappend= (+)
  mempty= 0


