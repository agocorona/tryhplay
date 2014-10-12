{-# LANGUAGE OverloadedStrings #-}
import Haste.HPlay.View
import Haste.HPlay.Cell
import Haste
import Haste.LocalStorage
import Haste.Serialize
import qualified Data.Map as M
import Control.Applicative
import Data.Maybe
import Control.Monad.IO.Class

import Haste.JSON (JSON(..))

main= runBody $ do
   player1 <- "player1: " ++> getString Nothing `validate` nonEmpty `fire` OnChange
   player2 <- "player2: " ++> getString Nothing `validate` nonEmpty `fire` OnChange
   return (player1, player2)
  `wcallback`  mainPage 

mainPage :: (String, String) -> Widget ()
mainPage players= do
  scores <-  liftIO $ getItem "match" `onLeft` return emptyMatch
  mapM_ (line players) scores

line (player1, player2) score=
  total fst score  ++> inc player1 ++> player1 
  ++> player2 ++>  inc player2 <++ total snd score 

total player score = do
  match <- getItem "match" `onLeft` return emptyMatch
  return $ fromMaybe (error  "total error") player M.lookup score $ scores match

nonEmpty s = return $ if null s then Just "non empty, please" else  Nothing

data Match= Match{player1, player2 :: String, scores :: M.Map String (Int,Int)} 
             deriving (Show,Eq,Read)

emptyMatch= Match "" "" M.empty

onLeft comp def= do
    r <- comp
    case r of
      Left _ -> return def
      Right r -> return r

instance Serialize Match where
  toJSON= Str . toJSString . show
  parseJSON (Str jss)=  return . read  $ fromJSStr jss
  
inc player score= do
  input ! atr "type" "submit" ! atr "value" "+" `pass` OnClick
  match <- getItem "match" `onLeft` return emptyMatch
  let score = fromMaybe (error $ "inc "++ "score error") $ M.lookup score $ scores match
  let val= undefined
  setItem "match" match{scores= M.insert player val match}