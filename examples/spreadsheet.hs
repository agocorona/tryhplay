-- Not yet finished

import Haste.Foreign
import Haste
import Haste.HPlay.View
import Haste.HPlay.Cell as Cell
import Control.Applicative
import Data.Monoid
import Data.IntMap as M
import Control.Monad.IO.Class
import System.IO.Unsafe

main= do
-- export "c" getCell
 runBody $ do
   wraw $ p << "spreadsheet" 
   ss <- rows `fire` OnKeyUp
   let r =   Prelude.map (\exp ->  tojs ss exp)  ss
   -- let r = loeb $  Prelude.map (\exp -> (\exps ->ueval $ tojs exps exp) )  ss
   
   print r
 --  wraw $ p $ show res
   
   
--   wraw $ p $ show $ loeb [\exps -> length exps,\exps -> x!!0]
  -- r <- liftIO $ evalFormula "with ({num: 2, i:3}){num*i}"
 
 where
 print = wraw . pre
 tojs :: [String] -> String -> String
 tojs xs exp= "with({"++tojs' 0 xs++"}){"++exp++"}"
   where 
   tojs' _ []= ""
   tojs' n (x:xs)='c': show (n `quot` 2)++ show (n `rem` 2)
                    ++ ": '"++ x++"', "++tojs' (n+1) xs
 rows= mconcat[tr <<< row x | x <- [1..2]]
 row x=mconcat[td <<< mkcell x y | y <-[1..2]]

--   r  <- Cell.get entry >>= return . reverse
--   revEntry .= r

cell :: Int -> Int -> Cell String
cell x y=
  let ide= getId x y
  in boxCell ide 

mkcell :: Int -> Int -> Widget [String]
mkcell x y= mk (cell x y) (Just "0") >>= \x -> return [x]
  

evalFormula :: String  ->  IO String
evalFormula=  ffi $ toJSString 
      "(function(exp){try{ return eval(exp);}catch(e){return '0'}})"

ueval= unsafePerformIO . evalFormula
   

getId x y= show x ++ show y

-- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
loeb x = fmap (\a -> a (loeb x)) x