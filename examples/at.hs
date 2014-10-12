-- relocating the output within a div using 'at'
-- classical presentation of a sum of numbers
-- Also uses session data to store the sum
-- And includes style formatting
-- Uses Cell boxes to erase the box when the number is summed up

{-# LANGUAGE NoMonomorphismRestriction #-}
import Haste.HPlay.View
import Haste.HPlay.Cell
import Control.Applicative
import Prelude hiding(id,div)

main= do
 -- add a style for all the div elemenents at the 
 addHeader $ estyle 
           ! atr "type" "text/css" 
           $ "div{margin-right:50%;text-align:right}"
           
 runBody $ do
    wraw $ div ! id "sums" $ noHtml
    wraw $ pr  "------------"
    wraw $ div ! id "result" $  p $ show (0 :: Int)
    
    let entry= boxCell "entry"
    r <- pr <<< mk entry  Nothing ! atr "size" "9"  `fire` OnChange
    sum <- getSData <|> return (0 :: Integer)
    let nsum= sum + r
    setSData $ sum +r
    entry .= ""
    at "sums"   Append $ wraw $ p  ("+" ++ show r)
    at "result" Insert $ wraw $ p  ("=" ++ show nsum)
         
pr= div  -- ! style "margin-right:50%;text-align:right"

-- define the tag "style" that is not defined in Haste.Perch
estyle st= nelem "style" `child` st