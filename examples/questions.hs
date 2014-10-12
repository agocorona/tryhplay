-- Fields of a form appear in sequence. 
-- Some of the fields trigger events instantly. Some others use a button to trigger them. 
-- It also contains option buttons, radio buttons etc
-- This formulary is the same than the one run in the server by MFlow
--    http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination
          
import Haste.HPlay.View
import Data.Monoid
import Control.Applicative

main= runBody questions

questions :: Widget ()
questions=  center <<< do -- PerchM monad
      p "Fields of a form appear in sequence. \
        \Some of the fields trigger events instantly. Some others use a button to trigger them. \
        \It also contains option buttons, radio buttons etc"
      p $ do
        toElem "This formulary is the same than the one "
        a ! href "http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"
          $ "run in the server by MFlow"
  ++>
   do
      (n,s) <- (,) <$> p << "Who are you? "
                   ++> getString Nothing <++ br 
                   <*> getString Nothing <++ br
                   <** submitButton "ok" `fire` OnClick
      
      flag <- br ++> getRadio[radiob "work?",radiob "study?"] <++ br

      r<- case flag of
         "work?" -> Left  <$> b << "do you enjoy your work? "
                              ++> getBool True "yes" "no"
                              <** submitButton "ok" `fire` OnClick <++ br

         "study?"-> Right <$> b << "do you study in "
                              ++> getRadio[radiob "University"
                                          ,radiob "High School"]

      p << ("You are "++n++" "++s)

        ++> case r of
             Left fl ->   p << ("You work and it is " ++ show fl ++ " that you enjoy your work")
                            ++> noWidget

             Right stu -> p << ("You study at the " ++ stu)
                            ++> noWidget

  where
  hint s= [("placeholder",s)]
  radiob s n=  wlabel  (fromStr s) $ setRadioActive s n
