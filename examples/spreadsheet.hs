-- Not yet finished

import Haste.HPlay.View
import Haste.HPlay.Cell as Cell
import Control.Applicative
import Data.Monoid
import Data.IntMap as M

main= runBody $ do
 export "c" getCell
 wraw $ p << "spreadsheet" 
 rows
 
 where
 rows= mconcat [tr <<< row y | y <- [1..3]]
 row y=mconcat[td <<< cell x y | x <-[1..3]]


--   r  <- Cell.get entry >>= return . reverse
--   revEntry .= r

cell :: Cell String
cell x y= do
  let ide= getId x y
  expr <- mk (boxCell ide Nothing `fire` onKeyPress
  dep <- dependencies expr
  r <- evalFormula expr
  map <- getSData <|> return M.empty
  setSessionData $ M.insert ide (r,dep) map


evalFormula :: String  -> IO Double
evalFormula= ffi $ toJSString "(function(exp){ return eval(exp);})"

dependencies str=

splits pat str= splits' [] str
splits s str | null str = (s,  "")
             | pat `isPrefixOf` str = (s, drop n str)
             | otherwise=   splits ( s++[ (head str)]) (tail str)
  where
  n= lenght pat
   
getCell x y= Cell.get $ getId x y

getId x y= show x ++ show y