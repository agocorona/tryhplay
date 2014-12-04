--  sum recursively n numbers, but remember the previous entries when one entry is edited

import Haste.HPlay.View
import Data.Monoid
import Control.Applicative
import qualified Data.Map as V

main= runBody sumCell

sumCell :: Widget ()

sumCell = p  "This widget sum recursively n numbers, but remember the\
          \ previos entries when one entry is edited" ++> sumr 0 0

 where
 sumr i r=do
     r' <- cell i
     b (show $ r+r') ++> br ++> return ()
     sumr (i +1) (r+r')

 cell i=  do
     stored <- Just <$> getNumber i <|> return Nothing
     r' <- inputInt stored `fire` OnKeyUp <|> fromM stored
     addNumber i r'
     return r'
   where
   addNumber i x= do
        xs <- getSData <|> return  V.empty
        setSData $ V.insert i x xs

   getNumber :: Int -> Widget Int
   getNumber i= do
        xs <- getSData
        case  V.lookup i xs of
          Nothing -> empty
          Just x  -> return x

   fromM Nothing = empty
   fromM (Just x) = return x


