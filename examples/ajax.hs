import Haste.HPlay.View

main= runBody $ do
     r <- ajax POST "/compile" [("name","test"),("text","main=print \"hello\"")]
     wprint (r :: Maybe String)