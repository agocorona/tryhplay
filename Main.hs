{-#LANGUAGE OverloadedStrings, DeriveDataTypeable, QuasiQuotes #-}
module Main where

import MFlow.Wai.Blaze.Html.All
import MFlow (mimeTable)
import Haste.Compiler
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
import Debug.Trace
import Data.Maybe
import Data.Char

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
                a ! href "//haste-lang.org" $ "Haste"
                " & "
                a ! href "//github.com/agocorona/playground" $ "HPlayground"
                " client-side framework."
            p $ "Create, compile to HTML+JavaScript and execute your Haskell programs in the browser"

          h3 <<< wlink "none"  "Create a new program"
            <|> h3 <<< (wlink ("git" :: String) "Compile a Haste project from a Git repository" `waction` fromGit)
            <|> h3 "Or you can modify one of these examples "
            ++> firstOf[handle e | e <- exampleList]


    if example== "noedit" then return () else do
     page $ do
     
      extext <- if example /= "none" then liftIO $ TIO.readFile $ projects ++ example else return ""
      (name',r)<- (wform  $
                   (,) <$> getString (Just example) <! [("placeholder","name")]
                       <++ (a ! href "/" $ " home")
                       <*> getMultilineText extext <! [("style","visibility:hidden"),("id","hiddenTextarea")]
                    <++ acedit
                    <** br
                    ++> submitButton "save & compile"
                    <++ br) <! [("onsubmit","return copyContent()")]
                    <++ executeEmbed (strip example ++ ".html")

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

--      r <- liftIO . shell $ inDirectory projects $ genericRun "/app/.cabal/bin/hastec" [hsfile,"--output-html"] ""
--      r <- liftIO . shell $ inDirectory projects $ genericRun "/home/user/.cabal/bin/hastec" [hsfile,"--output-html"] ""
      hastec <- liftIO $ findExecutable "hastec" `onNothing` error "hastec not foound"
      r <- liftIO . shell $ inDirectory projects $ genericRun hastec [hsfile,"--output-html"] ""
      case r of
        Left errs -> fromStr ("*******Failure: not found hastec: "++  errs) ++> empty
        Right (r,out,err) ->
          case r of
              True -> do
               let html= name ++ ".html"
               when (example /= name) $ wraw $ do
                  script $ "var elem=document.getElementById('exec');\
                           \elem.parentNode.removeChild(elem);"
                  executeEmbed html
               stop
 
              False -> stopAt $ do
                 script $ "var elem=document.getElementById('exec');\
                           \elem.parentNode.removeChild(elem);"

                 errorEmbed err

      <++ (p $ a ! href "/" $ "home")


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

  a ! href (fromString $ "/exec/"++ name ) $ "execute full page"

  iframe ! At.id "exec"
         ! At.style "position:relative;left:0%;top:10px;width:100%;height:100%"
         ! src (fromString $ "/exec/"++ name)
         $ mempty


errorEmbed err=
  div    ! At.style "position:fixed;left:50%;top:10%;width:50%;height:100%"
         $  pre $ fromStr err

extractDes code=unlines $ map (drop 2) . takeWhile ("--" `L.isPrefixOf`) $ lines code

stopAt= notValid

strip name'=
  let rname= reverse name'
  in  if "sh." `L.isPrefixOf` rname then reverse $ drop 3 rname else name'

handle :: Example -> View Html IO String
handle e= autoRefresh $ do
 let name'= exname e

 (wlink name' << name' <++ firstLine e) `wcallback`  const (showExcerpt e)

firstLine e=do
  div ! At.style "margin-left:5%" $ do
      toHtml $ L.takeWhile (/='\n') $ desc e
      "..."


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

            _       -> mapM_ (\l -> p ! At.style "margin-left:5%"
                         $ toHtml l) $ L.lines $ desc e

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
           Examples exampleList <- readDBRef examples !> "delete"
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
     Examples exampleList <- readDBRef examples !> "delete"
                      `onNothing` unsafeIOToSTM initExamples
     writeDBRef examples . Examples $ L.delete (Example fil undefined Local) exampleList
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
    let edited = Example (reverse . takeWhile (/='/')$ reverse urln) url (Git url)
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
