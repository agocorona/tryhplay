import Haste
import Haste.Perch
import Prelude hiding (div)

main :: IO ()
main = do
  body <- getBody
  flip build  body $ do
    div $ do
      addEvent this OnClick $ \_ _ -> alert "hello, world!"
      div $ do
        p "hello"
        p ! atr "style" "color:red" $ "world"
  return ()