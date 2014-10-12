-- Sum recursively n numbers. When enters 0, present the result
import Haste.HPlay.View

main= runBody sumRecursive

sumRecursive :: Widget ()
sumRecursive = p  "This widget sum recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=
    (inputInt Nothing `fire` OnKeyUp) >>= \ r' ->
    if r'== 0
      then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
      else 
        (b (show $ r+r') ++> br ++> return ()) >>
        sumr (r+r')