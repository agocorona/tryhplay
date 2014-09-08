-- adapted from the concurrency example form haste at:
-- https://github.com/valderman/haste-compiler/tree/master/examples
-- In this version the HTML is created programatically with the perch library
import Haste
import Haste.Concurrent hiding ((!))
import Haste.Perch
import Prelude hiding(span,id)

main = do
 body <- getBody
 (flip  build)  body $ do
   center ! style "margin-top: 10em; font-face: trebuchet; font-size: 16pt;" $ do
       span "Hey, look, a counter:"
       span ! id "counter" $ "0"

 concurrent $ do
  ctr <- newEmptyMVar
  forkIO $ updater ctr
  counter ctr 0

-- | This thread counts.
counter :: MVar Int -> Int -> CIO ()
counter ctr startval =
    go startval
  where
    go n = do
      putMVar ctr n
      wait 1000
      go (n+1)

-- | This thread updates the GUI.
updater :: MVar Int -> CIO ()
updater ctr =
    withElem "counter" $ go
  where
    go e = do
      x <- takeMVar ctr
      setProp e "innerHTML" (show x)
      go e



