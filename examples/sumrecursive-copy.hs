-- Sum recursively n numbers. When enters 0, present the result
import Haste.HPlay.View

main= runBody sumRecursive

sumRecursive :: Widget ()
sumRecursive = p  "This widget sum recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=do
    r' <- inputInt Nothing `fire` OnKeyUp
    if r'== 0
      then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
      else do
        b (show $ r+r') ++> br ++> return ()
        sumr (r+r')