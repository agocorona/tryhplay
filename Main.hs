{-#LANGUAGE OverloadedStrings, DeriveDataTypeable, QuasiQuotes #-}
module Main where

import MFlow.Wai.Blaze.Html.All
import MFlow (mimeTable)
import Data.Default
import Prelude hiding (id,div,head,span)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.String
import Data.TCache.DefaultPersistence
import Data.TCache.IndexText

import System.Directory
import System.IO.Unsafe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as SB
import qualified Data.Text.Lazy as TL
import Data.Typeable
import Data.Monoid
import Text.Blaze.Html5.Attributes as At hiding (step,name)
import qualified Text.Blaze.Html5 as El
import Control.Monad
import Control.Shell
import Text.Hamlet

import Data.Maybe
import Data.Char

import Network.WebSockets as WS
import qualified Data.Text as T
import qualified Control.Concurrent as C

--import Debug.Trace
--(!>)= flip trace


projects= "./examples/"

type Url = String

data Source = Local | Git Url deriving (Read,Show)

data Example= Example{exname, desc :: String, source:: Source, original :: Bool} deriving (Read,Show,Typeable)

instance Eq Example where
  Example n _ _ _ == Example n' _ _ _= n == n'


data Examples= Examples [Example]  deriving (Read,Show,Typeable)
instance Indexable Examples where key = const "examples"
instance Serializable Examples where
  serialize = B.pack . show
  deserialize = read . B.unpack

listExamples (Examples list)= list

examples= getDBRef "examples"

initExamples= do
  ss <- getDirectoryContents projects
  let sources = filter (".hs" `L.isSuffixOf`) ss
  exs <- mapM (\s -> readFile (projects ++ s) >>= \t -> return (Example s (extractDes t) Local True) ) sources
  return $ Examples exs

application ::  WS.ServerApp
application  pending = do
   conn <- WS.acceptRequest pending
   loop conn
   where
   loop conn= do
     msg <- WS.receiveData conn :: IO T.Text

     case msg of
       "hello" ->   WS.sendTextData conn ("hello back" :: T.Text)
       _ -> do
             exs <- liftIO $ atomically (readDBRef examples)
                                  `onNothing` initExamples
             WS.sendTextData conn  $ T.pack $ show exs
     loop conn
main= do
  C.forkOS $ do

     WS.runServer "0.0.0.0" 24601 $ application



  indexList listExamples (map TL.pack . map exname )

  addMessageFlows [("exec", wstateless serveOutputRest)
                  ,("compile", wstateless compileServ)]
  runNavigation "try" . transientNav $ do
    example <- page $   do
          Examples exampleList <- liftIO $ atomically (readDBRef examples)
                                  `onNothing` initExamples
          wraw $ do
            h1 $ do
                "Try "
                a ! href "//haste-lang.org" $ "Haste"
                " & "
                a ! href "//github.com/agocorona/playground" $ "HPlayground"
                " client-side framework."
            p $ "Create, compile to HTML+JavaScript and execute your Haskell programs in the browser"
            p $ do
              "Run this locally or create your own heroku instance: "
              a ! href "//github.com/agocorona/tryhplay" $ "code & docs"
            p $ "Save your compliled program  (HTML+Javascript)  by downloading the HTML generated page\
                   \ , which contains all the JScript necesary for running it locally"
            p $ b "Thanks to Anton Ekblad for his wonderful haste compiler"
            p $ b "NOTE: this IDE does not execute Haste programs with server side (using Haste.App)"
            p $ b "NOTE 2: From time to time Heroku reset the instance and erase your files"
          h3 <<< wlink "none"  "Create a new program"
            <|> h3 <<< (wlink ("git" :: String) "Compile a Haste project from a Git repository" `waction` fromGit)
            <|> h3 "Or you can modify and rename one of these examples:"
            ++> firstOf[handle e | e <- exampleList]




    if example== "noedit" then return () else do
     page  $  do
--      requires [JScriptFile jqueryScript [ "$(document).ready(function(){$(\"form\").submit(function(event) {alert(\"hi\");return copyContent()})})"]]

      do
       extext <- if example /= "none" then liftIO $ TIO.readFile $ projects ++ example else return ""
       mr<- (wform  $ Just <$> (
                    (,) <$> "Please rename if you do major changes >"
                       ++> getString (Just example) <! [("placeholder","set name")]
                       <*> (submitButton "save & compile"
                       <++ (a ! href "/" ! class_ "_noAutoRefresh" $ " home")
                       **> getMultilineText extext <! [("style","visibility:hidden")
                                                      ,("id","hiddenTextarea")])
                       <++ acedit
                       <> br)) <! [("onsubmit","return copyContent()")]
                  <|> return Nothing

--      wraw $ script ! type_ "text/javascript" $ "function code(e) {\n\
--                            \e = e || window.event;\n\
--                            \return(e.keyCode || e.which);\n\
--                        \}\n\
--                        \window.onload = function(){\n\
--                            \document.onkeypress = function(e){\n\
--                                \var key = code(e);\n\
--                                \alert (key);\n\
--                            \};\n\
--                        \};"
       case mr of
         Nothing -> stopAt $ executeEmbed (strip example ++ ".html")

         Just (name',r) -> do
           name <-  newName name'
           r <- compileIt name $ T.unpack r
           case r of
            Left errs -> fromStr ("*******Failure: not found hastec: "++  errs) ++> empty
            Right (r,out,err) ->
              case r of
                  True -> do
                   let html= name ++ ".html"
--                   when (example /= name) $
                   wraw $ executeEmbed html
                   stop

                  False -> stopAt $ errorEmbed err




--jqueryScript= getConfig "cjqueryScript" "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"

strip name'=
  let rname= reverse name'
  in  if "sh." `L.isPrefixOf` rname then reverse $ drop 3 rname else name'


newName name'= do
      let name= strip name'
          hsfile = name ++ ".hs"
      Examples exampleList <- liftIO $ atomically $ readDBRef examples
                      `onNothing` unsafeIOToSTM initExamples
      let mr = filter ( (hsfile ==) . exname) exampleList
      return $ case mr of
         [f] -> if original f then  (name++"-copy") else name
         _   -> name


compileServ= do
    name' <- wparam  "name"
    text <- wparam "text"
    disp $  compileIt name' text
   <?>  "Left \"parameter  error\""

compileIt :: String -> String ->View Html IO (Either String(Bool,String,String))
compileIt name' code'= do
      let name= strip name'
          hsfile = name ++ ".hs"
          code= filter (/= '\r')  code'
          des= extractDes code

      Examples exampleList <-  liftIO $ atomically $ readDBRef examples
                      `onNothing` unsafeIOToSTM initExamples

      liftIO $ writeFile  (projects ++ hsfile) code
      let edited= Example (name ++ ".hs") des Local False
      liftIO $ atomically $ writeDBRef examples . Examples . L.nub $ edited:exampleList
      hastec <- liftIO $ findExecutable "hastec" `onNothing` error "hastec not foound"
      liftIO . shell $ inDirectory projects $ genericRun hastec [hsfile,"--output-html"] ""


serveOutputRest= do
    mfile <- getPath []
    case mfile of
       "" -> wraw $ b  "not found"
       file -> serveOutput file

    where
    getPath segs= do
      mseg <- getRestParam
      case mseg of
        Nothing  -> return segs
        Just seg -> getPath (segs++"/"++seg)


serveOutput name= do
    let file = projects ++ name
        ext= reverse. takeWhile (/='.') $  reverse file
    content <- liftIO $ B.readFile file

    setHttpHeader "Content-Type" $ case lookup ext mimeTable of
            Just mime -> mime
            Nothing -> "text/plain"
    t <- liftIO $ getModificationTime file
    etag  . SB.pack $ show t
    maxAge 3600
    rawSend content

executeEmbed "none.html"= mempty
executeEmbed name=
 div ! At.style "position:fixed;left:50%;top:0%;width:50%;height:100%" $ do
  button ! At.id "hide" ! onclick "var el= document.getElementById('exec');\
                     \var st= el.style.visibility;\
                     \el.style.visibility= st=='hidden'? 'visible':'hidden' ;\
                     \document.getElementById('editor').style.width=st=='hidden'?'50%':'100%';"
         $ "hide/show"
  button ! onclick "document.getElementById('exec').src=document.getElementById('exec').src;" $ "reload"

  a ! href (fromString $ "/exec/"++ name ) $ " execute full page"

  iframe ! At.id "exec"
         ! At.style "position:relative;left:0%;top:10px;width:100%;height:90%"
         ! src (fromString $ "/exec/"++ name)
         $ mempty


errorEmbed err=
  div ! At.style "position:fixed;left:50%;top:0%;width:50%;height:100%" $ do
  button ! At.id "hide" ! onclick "var el= document.getElementById('exec');\
                     \var st= el.style.visibility;\
                     \el.style.visibility= st=='hidden'? 'visible':'hidden' ;\
                     \document.getElementById('editor').style.width=st=='hidden'?'50%':'100%';"
         $ "hide/show"

  div    ! At.id "exec"
         ! At.style "position:fixed;left:50%;top:10%;width:50%;height:100%"
         $  pre $ fromStr err

extractDes code=unlines $ map (drop 3) . takeWhile ("--" `L.isPrefixOf`) $ lines code

stopAt= notValid


handle :: Example -> View Html IO String
handle e= autoRefresh $ firstLine e `wcallback`  const (showExcerpt e)
 where
 firstLine :: Example -> View Html IO String
 firstLine e=
   let name'= exname e in do
      wraw $ toHtml name'
      div ! At.style "margin-left:5%" <<< do
         pre <<< do
            wraw << (L.take 80 (desc e) ++"...\n")
            wlink name' "more"

showExcerpt e= do
     let name'= exname e
         name = strip name'
         html = case Main.source e of
                   Git _ -> desc e
                   _ -> name ++ ".html"

     compiled <- liftIO $ doesFileExist $ projects ++ html
     wraw $ do
           b  << name'
           ": "
           case Main.source e of
            Git url -> p ! At.style "margin-left:5%"
                         $ a ! class_ "_noAutoRefresh" ! target "_blank" ! href (fromString $ url) $ "see the Git repository for information"

            _       -> div ! At.style "margin-left:5%" $ pre << desc e -- mapM_ (\l -> p ! At.style "margin-left:5%"
                        -- $ pre $  toHtml l) $ L.lines $ desc e

     p ! At.style "margin-left:5%"
        <<< (maybeExecute compiled html
        **> " | "
        ++> maybeEdit e name'
        <++ " | "
        <|> " "
        ++> do wlink ("delete" :: String) "delete" <! noAutoRefresh <++ br
               deletef e)

 where
 maybeExecute compiled html= when compiled $
        (stopAt $ a ! href (fromString $ "/exec/"++ html ) $ "execute alone") <! noAutoRefresh


 maybeEdit e name'= case Main.source e  of
                 Git repo -> do
                         wlink ("comp":: String) "Actualize/compile/execute" <! noAutoRefresh
                         `waction` const (page $ comp e )
                 _ ->  do
                         wlink ("edit" :: String) " edit & compile & execute " <! noAutoRefresh
                         return name'

comp e = do
     mcabalCont <- liftIO $ do
          git <- findExecutable "git" `onNothing` error "git not found"
          hasteInst <- findExecutable "haste-inst" `onNothing` error "haste-inst not found"
          r <- shell $ inDirectory projects $ do
                  genericRun git ["pull"] ""
                  inDirectory (exname e) $ do
                        genericRun hasteInst ["configure"] ""
                        genericRun hasteInst ["build"] ""
          case r of
            Left errs -> return $ Left   errs
            Right (r,out,err) -> do
              case r of
                False -> return $ Left (out ++ err)
                True -> do
                   cabal <- findCabal $ (projects ++ exname e)
                   r <- SB.readFile (projects ++ exname e ++"/"++ cabal)
                   return $ Right r
     case mcabalCont of
      Left errs -> pre << errs ++> empty
      Right cabalCont -> do
         let (r1,r2)=  SB.breakSubstring "main-is:" cabalCont
             file = SB.tail $ SB.takeWhile (/= '.') $ SB.drop 8 r2
             sdirs = getCabalVar "hs-source-dirs:" cabalCont
             dirs = getDirs sdirs
             f dir= do
               fs <- getDirectoryContents $ projects ++ exname e ++"/"++dir       -- !> dir
               return $ if not . null $ filter (== ( SB.unpack file++".html")) fs then Just dir else Nothing

         [dir]<- liftIO $ return . catMaybes =<<  mapM f dirs                     -- !> show dirs

         let html=  exname e ++ "/"++ dir ++ "/"++  SB.unpack file ++ ".html"
         liftIO $ atomically $ do
           Examples exampleList <- readDBRef examples -- !> "delete"
                      `onNothing` unsafeIOToSTM initExamples


           writeDBRef examples . Examples $ L.nubBy (\ x y -> exname x==exname y)
                                          $ [e{Main.desc= html }] ++ exampleList

         stopAt $ do
           p "actualized and compiled "
           a ! href (fromString $ "/exec/"++  html) $ "Run it"
     where
     getDirs cont | SB.null cont= []
                  | otherwise=
                      let (dir,rest) = SB.break sep . SB.dropWhile  sep $ cont
                      in SB.unpack dir : getDirs rest
                  where
                  sep x= isSeparator x || x=='\r'

     getCabalVar var cont=
         let (r1,r2)=  SB.breakSubstring var cont
         in SB.tail $ SB.takeWhile (/= '\n') $ SB.drop (SB.length var) r2

     findCabal dir= do
       fs <- getDirectoryContents dir
       let r = L.filter (L.isSuffixOf ".cabal") fs
       case r of
         [f] -> return f
         [] -> error $ exname e ++": cabal file not found"
         _  -> error $ exname e ++": duplicate cabal file"

deletef e  = liftIO $ do
   let fil= exname e

   liftIO $ atomically $ do
     Examples exampleList <- readDBRef examples
                      `onNothing` unsafeIOToSTM initExamples
     writeDBRef examples . Examples $ L.delete (Example fil undefined Local False) exampleList
   case Main.source e of
     Git _ -> removeDirectoryRecursive  $ projects ++ fil
     _     -> removeFile $ projects ++ fil
   return "noedit"


fromGit _ = ask $ do
    url <- getString Nothing <![("size","80"),("placeholder","URL of the git repository")]
           <** submitButton "import"
    git <- liftIO $ findExecutable "git" `onNothing` error "git not found"
    r <- liftIO . shell $ inDirectory projects $ genericRun git ["clone", url] ""
    let urln= map (\c -> if c== '\\' then  '/' else c) url
    let edited = Example (reverse . takeWhile (/='/')$ reverse urln) url (Git url) False
    case r of
        Left errs ->   pre << errs ++> empty
        Right (r,out,err) ->
          case r of
              False ->  pre << (out ++ err) ++> empty
              True -> do
                 liftIO $ atomically $ do
                    Examples exampleList <- readDBRef examples
                                  `onNothing` unsafeIOToSTM initExamples

                    writeDBRef examples . Examples . L.nub $ edited:exampleList
    comp edited
    p "imported. "++> wlink () "Press here to continue"
    return "noedit"


acedit = [shamlet|
 <style type="text/css" media="screen">
    #editor {
    width: 50%;
    height:89%
    }


 <div id="editor">


 <script src="//cdnjs.cloudflare.com/ajax/libs/ace/1.1.3/ace.js" type="text/javascript" charset="utf-8">
 <script>
    copyContent1();
    var editor = ace.edit("editor");
    // editor.setTheme("ace/theme/monokai");
    editor.getSession().setMode("ace/mode/haskell");
    //editor.getSession().on('change', heightUpdateFunction);
    //heightUpdateFunction();
    function heightUpdateFunction() {

        // http://stackoverflow.com/questions/11584061/
        var newHeight =
                  editor.getSession().getScreenLength()
                  * editor.renderer.lineHeight
                  + editor.renderer.scrollBar.getWidth()+ 100;


        document.getElementById("editor").style.height= newHeight.toString() + "px";
        editor.resize();
        }
    function copyContent () {
     document.getElementById("hiddenTextarea").value =
        editor.getSession().getValue();
     return true;
    }
    function copyContent1 () {
     document.getElementById("editor").innerHTML=
       document.getElementById("hiddenTextarea").value
        ;
     return true;
     }


 |]
