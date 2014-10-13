-- The browser side part of tryplayground coded as a single page hplayground app.
--
-- tryplayground is a MFlow application that run in the server side:
--
--     http://mflowdemo.herokuapp.com
--
-- The app generates the HTML in the server.
-- 
-- This example run a very similar  code in the browser using haste+perch+hplaygrund
-- it call a web service made in MFlow that compiles and send the result

import Haste.HPlay.View
import Haste hiding (onEvent)
import Haste.Prim
import Haste.Ajax hiding(textRequest)
import Haste.Foreign
import Control.Applicative
import Prelude hiding (id,div,map)
import Control.Monad.IO.Class
import Data.Monoid
import Data.List as L 
import System.IO.Unsafe
import Data.IORef

import Haste.App 
import qualified Haste.App.Concurrent as H
import qualified Control.Concurrent as C

import Debug.Trace
(!>) = flip trace

copyContent :: IO ()
copyContent= ffi $ toJSString  "(function(){if(typeof copyContent !== 'undefined')copyContent();})"


strip name'=
  let rname= reverse name'
  in  if "sh." `L.isPrefixOf` rname then reverse $ drop 3 rname else name'


result = unsafePerformIO $ newIORef def :: IORef String

trade' :: String -> IO String
trade' str= do
   runApp (mkConfig "ws://localhost:24601" 24601) $ do
     trade <- remote $ \x -> return x
 
     runClient $ do
        r <- onServer $ trade <.> str
        liftIO $ writeIORef result r
   readIORef result
   
main2= do  --test websockets
 -- body <- getBody
 -- (flip build) body $ input ! id "button" ! atr "type" "button" ! atr "value" "enter"

 
  --     \newmsg -> do
  --      message <- remoteMsg
  --      liftIO $ do oldmsg <- C.takeMVar message
  --              C.putMVar message newmsg
  --                  return oldmsg

  --   runClient $ withElem "button" $ \button -> do
  --      button `onEvent` OnClick $ \_ _ -> do
  --        newmsg <- prompt "Enter a message"
  --        oldmsg <- onServer $ trade <.> newmsg
  --        alert $ "The old message was: " ++ oldmsg
     
     
     runBody $ do
      input ! atr "type" "button" ! atr "value" "enter" `pass` OnClick
      liftIO $ do
        newmsg <- prompt "Enter a message"
        oldmsg <- trade' newmsg
        alert $ "The old message was: " ++ oldmsg

 

main= runBody $ do
    (name,text) <- (,)
        <$> controls
        <*> acedit
        
    wraw $ div  ! id "exec"
         ! style "position:fixed;left:50%;top:10%;width:50%;height:100%"
         $ noHtml
         
    liftIO $ postRequest  "/compile" [("name",  name),("text",text)] $ doit name
    <++ (p $ a ! href "/" $ "home")
 --   <> a ! href ("/exec/"++ name ) $ " execute full page"

    where

    doit _ Nothing= alert "Nothing"
    doit name (Just s)= do
      (flip runWidgetId) "exec" $ wraw $ do
          let resp= read s ::  Either String (Bool, String, String)
          case resp of
            Left errs -> errorEmbed ("*******Failure: not found hastec: "++  errs)
            Right (r,out,err) -> 
              case r of
                  True -> do
                   let html=  strip name ++ ".html"
                   executeEmbed html
                  False -> errorEmbed err
      return()

submitIt= submitButton "save & compile" `fire` OnClick 

controls :: Widget String
controls=  div ! style "position:fixed;left:0%;top:0%;width:100%;height:10%" <<< (
  fromStr "Please rename if changed"
  ++> getString (Just "test.hs") 
  <** submitIt  
  <++ do
    button  ! style "float:right"
            ! id "hide" 
            ! atr "onclick" "var el= document.getElementById('exec');\
                  \var st= el.style.visibility;\
                  \el.style.visibility= st=='hidden'? 'visible':'hidden' ;\
                  \document.getElementById('editor').style.width=st=='hidden'?'50%':'100%';"
            $ "hide/show"

    button  ! style "float:right"
            ! atr "onclick" "document.getElementById('exec').src=document.getElementById('exec').src;" $ "reload"
    a ! href "/" ! atr "class" "_noAutoRefresh" $ " home")


--executeEmbed "none.html"= noHtml
executeEmbed name=
  iframe ! style "position:relative;left:0;top:0;width:100%;height:100%"
         ! src ("/exec/"++ name)
         $ noHtml

errorEmbed err= pre  err

estyle c= nelem "style" `child` c

acedit = 
 liftIO copyContent 
 **> do
  div ! id "editor"
      ! style "position:absolute;top:10%;width: 50%"
      $ noHtml


  script ! src "http://cdnjs.cloudflare.com/ajax/libs/ace/1.1.3/ace.js"
         ! atr "type" "text/javascript"
         ! atr "charset" "utf-8"
         $ noHtml

  script  ! atr "type" "text/javascript" $
    "var editor;\
    \var textarea;\
    \function init(){\
     \textarea= document.getElementsByTagName('textarea')[0];\
     \copyContent1();\
     \editor = ace.edit('editor');\
     \editor.getSession().setMode('ace/mode/haskell');\
     \editor.getSession().on('change', heightUpdateFunction);\
     \heightUpdateFunction();\
    \function heightUpdateFunction() {\
        \var newHeight =\
                  \editor.getSession().getScreenLength()\
                  \* editor.renderer.lineHeight\
                  \+ editor.renderer.scrollBar.getWidth()+ 100;\
        \document.getElementById('editor').style.height= newHeight.toString() + 'px';\
        \editor.resize();\
        \};\
    \function copyContent1 () {\
     \document.getElementById('editor').innerHTML=textarea.value;\
     \return true;};}\
    \function copyContent () {\
     \textarea.value = editor.getSession().getValue();\
      \return true;\
    \};\
    \function waitAce(){\
     \if (typeof ace !== 'undefined') {init();}\
     \else{window.setTimeout(function(){waitAce();}, 10);}}\
    \waitAce();"

 ++> getMultilineText "" ! style "visibility:hidden"



-- | Make an AJAX request to a URL, treating the response as plain text.
postRequest :: URL
            -> [(Key, Val)]
            -> (Maybe String -> IO ())
            -> IO ()

postRequest url kv cb = do
  _ <- ajaxReq (toJSStr "POST") (toJSStr  url) True postData cb'
  return ()
  where
  cb' = mkCallback $ cb . fmap fromJSStr
  kv' = L.map (\(k,v) -> (toJSStr k,   encodeURI v)) kv
  postData = if null kv
             then toJSString ""
             else toQueryString kv' 

  encodeURI' :: String -> IO JSString
  encodeURI' =  ffi $ toJSString "(function(s){return encodeURIComponent(s)})"

  encodeURI= unsafePerformIO . encodeURI'





toQueryString :: [(JSString, JSString)] -> JSString
toQueryString = catJSStr (toJSString "&") . L.map (\(k,v) -> catJSStr (toJSString "=") [k,v])

foreign import ccall ajaxReq :: JSString    -- method
                             -> JSString    -- url
                             -> Bool        -- async?
                             -> JSString    -- POST data
                             -> JSFun (Maybe JSString -> IO ())
                             -> IO ()