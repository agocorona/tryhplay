import Haste.Perch
import Haste
import Haste.Foreign
import Data.Maybe
import Data.Monoid
import Control.Monad.IO.Class
import Prelude hiding (div)

main= do
  body <- getBody
  (flip build) body $ do
      div ! atr "class" "modify" $ "click"
      div $ "not changed"
      div ! atr "class" "modify" $ "here"
      
      addEvent this OnClick $ \_ _ -> do
          forElems' ".modify" $ this ! style "color:red" `child` " modified"

