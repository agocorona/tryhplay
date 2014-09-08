-- links, radio buttons, option buttons etc

import Haste.HPlay.View
import Control.Applicative
import Data.Monoid

main= runBody $ buttons <|> linksample

linksample= br ++> wlink "Hi!" (toElem "This link say Hi!")`fire` OnClick >>= \r -> wraw( b (" returns "++ r))

buttons= p "Different input elements:" ++> checkButton
                                       **> br ++> br
                                       ++> radio
                                       **> br ++> br
                                       ++> select
                                       <++ br
    where
    checkButton=do
       rs <- getCheckBoxes(
                       ((setCheckBox False "Red"    <++ b "red")   `fire` OnClick)
                    <> ((setCheckBox False "Green"  <++ b "green") `fire` OnClick)
                    <> ((setCheckBox False "blue"   <++ b "blue")  `fire` OnClick))
       wraw $ fromStr " returns: " <> b (show rs)

    radio= do
       r <- getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]

       wraw $ fromStr " returns: " <> b ( show r )

    select= do
       r <- getSelect (   setOption "red"   (fromStr "red")  

                      <|> setOption "green" (fromStr "green")
                      <|> setOption "blue"  (fromStr "blue"))
              `fire` OnClick

       wraw $ fromStr " returns: " <> b ( show r )
 