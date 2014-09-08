module Main where
import Haste
import Haste.Perch
import Prelude hiding (id,span)

main = do
  body <- getBody
  (flip build) body $ do
    center ! style "margin-top: 10em; font-face: trebuchet; font-size: 16pt;" $ do
      input ! atr "type" "text" ! atr "size" "4" ! id "a" ! style "font-size: 16pt;" ! value "0"
      select ! id "op" ! style "font-size: 16pt;" $ do
        option ! value "+" $ "+"
        option ! value "-" $ "-"
        option ! value "*" $ "*"
        option ! value "/" $ "/"
  
      input ! atr "type" "text" ! atr "size" "4" ! id "b" ! style "font-size: 16pt;" ! value "0"
      b "="
      span ! id "result" $ "0"
  
  withElems ["a","b","op","result"] calculator


value= atr "value"

calculator [a,b,op,result] = do
    
    onEvent a  OnKeyUp $ \_ -> recalculate
    onEvent b  OnKeyUp $ \_ -> recalculate
    onEvent op OnChange $ recalculate
    return ()
  where
    recalculate = do
      ma <- getValue a
      mb <- getValue b
      Just op' <- getValue op
      case (ma, mb) of
        (Just a', Just b') -> setProp result "innerHTML" (toString $ (calc op') a' b')
        _                  -> return ()

    calc "+" = (+)
    calc "-" = (-)
    calc "*" = (*)
    calc "/" = (/)
    calc _   = \_ _ -> 0 :: Double
    


