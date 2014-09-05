-- Not yet finished

import Haste.HPlay.View
import Haste.HPlay.Cell as Cell
import Control.Applicative
import Data.Monoid

main= runBody $ do
 wraw $ p << "spreadsheet" 
 rows
 
 where
 rows= mconcat [tr <<< row y | y <- [1..3]]
 row y=mconcat[td <<< mk (boxCell (show x++show y) :: Cell String) Nothing | x <-[1..3]]


--   r  <- Cell.get entry >>= return . reverse
--   revEntry .= r

