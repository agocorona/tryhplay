{-#LANGUAGE OverloadedStrings, DeriveDataTypeable, QuasiQuotes #-}
module Main where

import MFlow.Wai.Blaze.Html.All
import Haste.Compiler
import Data.Default
import Prelude hiding (id,div,head)
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
import qualified Data.Text as T
import Data.Typeable
import Data.Monoid
import Text.Blaze.Html5.Attributes as At hiding (step,name)
import qualified Data.Text.Lazy as TL
import Control.Monad
import Control.Shell
import Text.Hamlet
import Debug.Trace



(!>)= flip trace

projects= "./examples/"

type Url = String

data Source = Local | Git Url deriving (Read,Show)

data Example= Example{exname, desc :: String, source:: Source} deriving (Read,Show,Typeable)

instance Eq Example where
  Example n _ _ == Example n' _ _ = n == n'

data Examples= Examples [Example] deriving (Read,Show,Typeable)
instance Indexable Examples where key = const "examples"
instance Serializable Examples where
  serialize = B.pack . show
  deserialize = read . B.unpack

listExamples (Examples list)= list

examples= getDBRef "examples"

initExamples= do
  ss <- getDirectoryContents projects
  let sources = filter (".hs" `L.isSuffixOf`) ss
  exs <- mapM (\s -> readFile (projects ++ s) >>= \t -> return (Example s (extractDes t) Local) ) sources
  return $ Examples exs


main= do
  indexList listExamples (map TL.pack . map exname )


  addMessageFlows [("exec", wstateless serveOutputRest)]
  
  runNavigation "try" . transientNav $ do
    example <- page $ do
          Examples exampleList <- liftIO $ atomically (readDBRef examples)
                                  `onNothing` initExamples
          wraw $ do
            h1 $ do
                "Try "
                a ! href "//github.com/agocorona/playground" $ "Playground"
            p $ "The haskell client-side framework."
            p $ "Create, compile to HTML+JavaScript and execute your programs in the browser"
            h2 "you can use one of these examples "

          firstOf[handle e | e <- exampleList]
              <|> h2 <<< wlink "none"  "or create a new program"
              <|> h2 <<< (wlink ("git" :: String) "Compile a Haste/Hplayground project from a Git repository" `waction` fromGit)


    if example== "noedit" then return () else do
     page $ wlink () "home" <|> do
     
      extext <- if example /= "none" then liftIO $ TIO.readFile $ projects ++ example else return ""
      (name',r)<- (wform  $
                  p <<< (( (,) <$> getString (Just example) <! [("placeholder","name")]
                               <*> getMultilineText extext <! [("style","visibility:hidden"),("id","hiddenTextarea")]
                    <++ acedit
                    <** br
                    ++> submitButton "save & compile"
                    <++ br))) <! [("onsubmit","return copyContent()")]


      let name= strip name'
          hsfile = name ++ ".hs"
          code= filter (/='\r') $ T.unpack r
          des= extractDes code

      liftIO $ writeFile  (projects ++ hsfile) code
      let edited= Example (name ++ ".hs") des Local
      liftIO $ atomically $ do
        Examples exampleList <- readDBRef examples
                      `onNothing` unsafeIOToSTM initExamples
        writeDBRef examples . Examples . L.nub $ edited:exampleList

      r <- liftIO . shell $ inDirectory projects $ genericRun "/app/.cabal/bin/hastec" [hsfile,"--output-html"] ""
--      r <- liftIO . shell $ inDirectory projects $ genericRun "/home/user/.cabal/bin/hastec" [hsfile,"--output-html"] ""
--      r <- liftIO . shell $ inDirectory projects $ genericRun "hastec" [hsfile,"--output-html"] ""
      case r of
        Left errs -> fromStr ("*******Failure: not found hastec: "++  errs) ++> empty
        Right (r,out,err) ->
          case r of
              True -> do
               wraw $ executeEmbed name
               notValid $ do
                let html= name ++ ".html"
                p $  b "compilation sucessful! "
                a ! href (fromString $ "/exec/"++ name ++ ".html" ) $ "execute full page"
 
              False ->  notValid $ errorEmbed err

      <|> p <<< wlink () "home"


serveOutputRest= do
    mfile <- getRestParam
    case mfile of
       Just file -> serveOutput file
       _ -> wraw $ b  "not found"

serveOutput name= do
    let file = projects ++ name
    content <- liftIO $ B.readFile file
    resetCachePolicy
    setHttpHeader  "Content-Type" "text/html"
    t <- liftIO $ getModificationTime file
    etag  . SB.pack $ show t
    maxAge 3600
    rawSend content



executeEmbed name=
  iframe ! At.style "position:absolute;left:50%;top:0%;width:50%;height:100%"
         ! src (fromString $ "/exec/"++ name ++ ".html" )
         $ mempty

errorEmbed err=
  div    ! At.style "position:absolute;left:50%;top:0%;width:50%;height:100%"
         $  pre $ fromStr err

extractDes code=unlines $ map (drop 2) . takeWhile ("--" `L.isPrefixOf`) $ lines code

strip name'=
  let rname= reverse name'
  in  if "sh." `L.isPrefixOf` rname then reverse $ drop 3 rname else name'

handle :: Example -> View Html IO String
handle e= do
 let name'= exname e

 (wlink name' << name' <++ br) `wcallback`  const (showExcerpt e)

showExcerpt e= do
     let name'= exname e
         name = strip name'
         html = projects ++ name ++ ".html"

     compiled <- liftIO $ doesFileExist html
     wraw $ do
           b  << name'
           ": "
           mapM_ (\l -> p ! At.style "margin-left:5%" $ toHtml l) $ L.lines $ desc e

     p ! At.style "margin-left:5%"
        <<< (maybeExecute compiled name
        **>  " " ++>  do wlink ("edit" :: String) "edit"
                         return name'

        <|>  " " ++>  do wlink ("delete" :: String) "delete" <++ br
                         deletef name')

 where
 maybeExecute compiled name= when compiled $ do
               wlink ("execute" :: String) "execute"
               serveOutput $ name ++ ".html"

-- maybeEdit e= if


 deletef fil  = liftIO $ do
   removeFile $ projects ++ fil
   liftIO $ atomically $ do
     Examples exampleList <- readDBRef examples !> "delete"
                      `onNothing` unsafeIOToSTM initExamples
     writeDBRef examples . Examples $ L.delete (Example fil undefined Local) exampleList
   return "noedit"


fromGit _ = ask $ do
    url <- getString Nothing <![("placeholder","URL of the git repository")]
    liftIO $ atomically $ do
        Examples exampleList <- readDBRef examples
                      `onNothing` unsafeIOToSTM initExamples
        let edited = Example (reverse . takeWhile (/='/')$ reverse url) url (Git url)
        writeDBRef examples . Examples . L.nub $ edited:exampleList
    liftIO . shell $ inDirectory projects $ genericRun "git" ["clone", url] ""
    return "noedit"


acedit = [shamlet|
 <style type="text/css" media="screen">
    #editor {
    width: 50%;
    }


 <div id="editor">


 <script src="//cdnjs.cloudflare.com/ajax/libs/ace/1.1.3/ace.js" type="text/javascript" charset="utf-8">
 <script>
    copyContent1();
    var editor = ace.edit("editor");
    // editor.setTheme("ace/theme/monokai");
    editor.getSession().setMode("ace/mode/haskell");
    editor.getSession().on('change', heightUpdateFunction);
    heightUpdateFunction();

    function heightUpdateFunction() {

        // http://stackoverflow.com/questions/11584061/
        var newHeight =
                  editor.getSession().getScreenLength()
                  * editor.renderer.lineHeight
                  + editor.renderer.scrollBar.getWidth();

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
