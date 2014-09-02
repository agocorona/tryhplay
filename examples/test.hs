-- This program will ask the user for her name, and echo a greeting back into
-- this text box as a comment.

{-# LANGUAGE OverloadedStrings #-}
import Haste
import Haste.Foreign
import Haste.Prim
import Haste.HPlay.View 
import Control.Applicative
import Data.Typeable
import Unsafe.Coerce


main2= do
  runBody $ do
    r <- wlink True "true" <|> br ++> wlink False "false"
    wraw $ p << r

wlink1 x v= do
    (a ! href ("#/"++show1 x)   $ v) `pass` OnClick
    return x
    
   where
   show1 x | typeOf x== typeOf (undefined :: String) = unsafeCoerce x
           | otherwise= show x


main=  runBody $ filters "all" 

filters op= (links op)
      `wcallback` (\op' -> do
         alert op'
         filters op')
    
    where
    links op= 
        wlink all         "All "   <|>
        wlink active      "Active "   <|>
        wlink completed   "Completed "  
    
    links2 op=
        inputSubmit active     <|>
        inputSubmit all        <|>
        inputSubmit completed   
    all= "All"
    active= "Active"
    completed= "Completed"