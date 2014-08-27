-- Two counters. One is pure and recursive, the other is stateful

{-# LANGUAGE  DeriveDataTypeable #-}

import Haste.HPlay.View
import Data.Typeable
import Control.Applicative

newtype Counter= Counter Int deriving Typeable

main= runBody $ counter 3

counter n = p "Two counters. One is pure and recursive, the other is stateful"
            ++> br ++> (center <<< (counter1 n <|> counter2 n))

 where
    -- a "pure" counter
    counter1 :: Int -> Widget ()
    counter1 n= (b (show n) ++> onemore)  `wcallback` (const $ counter1 $ n +1)

    onemore=  submitButton "+" `fire` OnClick


    -- a stateful counter
    counter2 n = do
      onemore -- isEmpty (getSData :: Widget Counter ) <|> (onemore >> return True)
      Counter n <- getSData <|> return (Counter n)
      wraw $ b (show n)
      setSessionData . Counter $ n +1


