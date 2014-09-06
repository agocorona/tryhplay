-- relocating the output within a div using 'at'
-- classical presentation of a sum of numbers
-- Also uses session data to store the sum
-- And includes style formatting

{-# LANGUAGE NoMonomorphismRestriction #-}
import Haste.HPlay.View
import Control.Applicative
import Prelude hiding(id,div)
import Control.Monad

main= do
 -- add a style for all the div elemenents at the <head>
 addHeader $ estyle 
           ! atr "type" "text/css" 
           $ "div{margin-right:50%;text-align:right}"
           
 runBody $ do
    wraw $ div ! id "sums" $ noHtml
    wraw $ pr  "------------"
    wraw $ div ! id "result" $  p $ show (0 :: Int)
    
    r <- pr <<< inputInt Nothing `fire` OnKeyPress
    EventData evname evdata <- getEventData
    
    when( evdata == Key 13) $ do
         sum <- getSData <|> return 0
         let nsum= sum + r
         setSData $ sum +r
         at "sums"   Append $ wraw $ p  ("+" ++ show r)
         at "result" Insert $ wraw $ p  ("=" ++ show nsum)
         
pr= div  -- ! style "margin-right:50%;text-align:right"

-- define the tag "style" that is not defined in Haste.Perch
estyle st= nelem "style" `child` st