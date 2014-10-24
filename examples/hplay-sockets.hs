import Haste.HPlay.View
import Haste.HPlay.WebSockets
import Control.Monad.IO.Class
import Haste
import Haste.Serialize
import Haste.JSON hiding ((!))
import Prelude hiding (div)
import Data.List as L
import Data.Monoid

main= runBody $ do
    ws <- wsOpen "ws://localhost:24601"
    req <- wbutton "hello" "send hello" <|> wbutton "other" "get files" <++ br
    case req of
     "hello" -> do
           resp <- wsAsk ws req 
           wprint (resp :: JSString)
     "other" -> do
        Examples examples <- wsAsk ws req
        wraw $ mconcat [handle e | e <- examples]

handle :: Example -> Perch
handle e=   firstLine e
 where
 firstLine :: Example -> Perch
 firstLine e=
   let name'= exname e in do
      b name'
      div ! style "margin-left:5%" $
         pre $  i (L.take 80 (desc e) ++"...\n")
            

instance  JSType Examples where
  toJSString = toJSString . show
  fromJSString jss=  let ms= fromJSString jss
                     in  maybeRead ms 
   where
   maybeRead Nothing = Nothing
   maybeRead (Just s)= case readsPrec 0 s of
       [(exs,"")] -> Just exs
       _ -> Nothing




data Examples= Examples [Example]  deriving (Read,Show)

data Source = Local | Git URL deriving (Read,Show)

data Example= Example{exname, desc :: String, source:: Source, original :: Bool}
     deriving (Read,Show)



