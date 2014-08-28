-- pascal triangle http://www.haskell.org/haskellwiki/Blow_your_mind

import Haste.HPlay.View
import Control.Applicative
import Data.Monoid

main= runBody $ showpascal 10

pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1] :: [[Int]]

showpascal n= p << ("Show " ++ show n ++ " rows of the Pascal triangle ")
   ++> mconcat[p ! style "text-align:center" $ row | row <- take n pascal]
   ++> empty   -- the applicative empty === noWidget
