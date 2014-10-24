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
   


main= runBody $ do
    (name,text) <- (,)
        <$> controls
        <*> acedit
        
    wraw $ div  ! id "exec"
         ! style "position:fixed;left:50%;top:10%;width:50%;height:100%"
         $ noHtml
         
    r <- ajax POST  "/compile" [("name",  name),("text",text)] 
    case r of
       Nothing -> error "ajax error"
       Just s ->  wraw $ do
          let resp= read s ::  Either String (Bool, String, String)
          case resp of
            Left errs -> errorEmbed ("*******Failure: not found hastec: "++  errs)
            Right (r,out,err) -> 
              case r of
                  True -> do
                   let html=  strip name ++ ".html"
                   executeEmbed html
                  False -> errorEmbed err

submitIt= submitButton "save & compile" `fire` OnClick 

controls :: Widget String
controls=  div ! style "position:fixed;left:0%;top:0%;width:100%;height:10%" <<< (
  fromStr "Please rename if changed"
  ++> getString (Just "test.hs") 
  <** submitIt ) 
--  <++ do
--    button  ! style "float:right"
--            ! id "hide" 
--            ! atr "onclick" "var el= document.getElementById('exec');\
--                  \var st= el.style.visibility;\
--                  \el.style.visibility= st=='hidden'? 'visible':'hidden' ;\
--                  \document.getElementById('editor').style.width=st=='hidden'?'50%':'100%';"
--            $ "hide/show"
--
--    button  ! style "float:right"
--            ! atr "onclick" "document.getElementById('exec').src=document.getElementById('exec').src;" $ "reload"
--    a ! href "/" ! atr "class" "_noAutoRefresh" $ " home")


executeEmbed :: String  -> Perch
executeEmbed "none.html"= mempty
executeEmbed name=
 div ! style "position:fixed;left:50%;top:0%;width:50%;height:100%" $ do
  button ! id "hide" ! atr "onclick" "var el= document.getElementById('exec');\
                     \var st= el.style.visibility;\
                     \el.style.visibility= st=='hidden'? 'visible':'hidden' ;\
                     \document.getElementById('editor').style.width=st=='hidden'?'50%':'100%';"
         $ "hide/show"
  button ! atr "onclick" "document.getElementById('exec').src=document.getElementById('exec').src;" $ "reload"

  a ! href ("/exec/"++ name ) $ " execute full page"

  iframe ! id "exec"
         ! style "position:relative;left:0%;top:10px;width:100%;height:90%"
         ! src  ( "/exec/" ++ name)
         $ noHtml


errorEmbed err=
  div ! style "position:fixed;left:50%;top:0%;width:50%;height:100%" $ do
  button ! id "hide" ! atr "onclick" "var el= document.getElementById('exec');\
                     \var st= el.style.visibility;\
                     \el.style.visibility= st=='hidden'? 'visible':'hidden' ;\
                     \document.getElementById('editor').style.width=st=='hidden'?'50%':'100%';"
         $ "hide/show"

  div    ! id "exec"
         ! style "position:fixed;left:50%;top:10%;width:50%;height:100%"
         $  pre $  err

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






