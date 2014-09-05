-- relocating the output within a div using 'at'
-- classical presentation of a sum of numbers
-- Also uses session data to store the sum

import Haste.HPlay.View
import Control.Applicative
import Prelude hiding(id,div)
import Control.Monad

main= runBody $ do
    
    wraw $ div ! id "sums" $ noHtml
    wraw $ p << "------------"
    wraw $ div ! id "result" $ (0 :: Int)
    
    r <- inputInt Nothing `fire` OnKeyPress
    EventData evname evdata <- getEventData
    
    when( evdata == Key 13) $ do
         sum <- getSData <|> return 0
         let nsum= sum + r
         setSData $ sum +r
         at "sums" Append $ wraw $ p << ( "+" ++ show r)
         at "result" Insert $  wraw $ p << ("=" ++ show nsum)
