-- Example of form validation with tree different presentation of error:
-- As a inline message, as an alert message and as a out of line message in
-- an external 
--
-- There is a second level validation that check for leap dates
--
-- Validation in hplayground can be done in the monadic sequence,as part of the logic
-- instead of using a special procedure like validate, but it is provided for compatibility
-- with other formlet frameworks
-- 


{-# LANGUAGE TypeSynonymInstances #-}
import Haste.HPlay.View
import Control.Monad.IO.Class
import Haste
import Prelude hiding (div,id)

main= runBody $ do
     wraw $ div ! id "yearerror" $ h3 "Validation example. enter a date: dd mm yyyy"
     d <- getDate

     wprint d

getDate= ((,,)   
    <$> inputInt Nothing    ! length_ "2" ! size "2"
          `validate` 
          (\d -> return (if d>= 1 && d <=31 
                          then Nothing else Just $ b "wrong day"))
    <*> inputInt Nothing  ! length_ "2" ! size "2"
          `validate` 
          (\m -> return (if m>=1 && m <= 12 
                          then Nothing 
                          else Just $ liftIO $ alert "wrong month"))
    <*> inputInt Nothing   ! length_ "4" ! size "4"
          `validate` 
           (\y -> do
              liftIO $ forElems' "#yearerror" $  clear
              return $ if y > 1900 && y <= 2014
                           then Nothing 
                           else Just $ forElems "#yearerror" $ do
                                           clear  -- perch sequence
                                           inred $ b "wrong year")
         
         
    <** inputSubmit "Ok" `fire` OnClick
    <++ br  )
                                                                                 
  `validate` (\date -> return $
                if leaperror date then Just $ b "wrong day" else Nothing)
                
    
length_= atr "length"
size= atr "size"
 
leaperror (d,m,y)= 
  let leap = leapyear y 
  in
    (leap && d > 29 && m == 2) ||
    ((not leap) && d >28 && m ==2)
  where
  leapyear y=  y `mod` 4==0 && ( y `mod` 100 /= 0 || y `mod` 400==0 )
  
