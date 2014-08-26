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
import qualified Data.ByteString.Lazy.Char8 as B
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

data Example= Example{exname, desc :: String} deriving (Read,Show,Typeable)

instance Eq Example where
  Example n _ == Example n' _ = n == n'

data Examples= Examples [Example] deriving (Read,Show,Typeable)
instance Indexable Examples where key = const "examples"
instance Serializable Examples where
  serialize = B.pack . show
  deserialize = read . B.unpack

listExamples (Examples list)= list

main= do
  indexList listExamples (map TL.pack . map exname )
  examples <- atomically $ newDBRef $
      Examples [ Example "example.hs" "muiltiple examples togeter"
               , Example "todo.hs" "the todo application as defined in todoMVC.com"
               , Example "sumtwonumbers.hs" "Sum thow numbers"] 

  setFilesPath projects
  let dontcare ="updateme"
  runNavigation "try" . transientNav $ do
    example <- page $ do
          Examples exampleList <- liftIO $ atomically $ readDBRef examples
                                  `onNothing` error "examples empty"
          wraw $ h1 "Try Playground"
          wraw $ h2 "you can load one of these examples "
          firstOf[handle e | e <- exampleList]
              <|> p <<< wlink "none"  "or create a new program"
         
    name <- page $ do
      extext <- if example /= "none" then liftIO $ TIO.readFile $ projects ++ example else return ""
      (name',r) <- (wform  $
                  p <<< (( (,) <$> getString (Just example) <! [("placeholder","name")]
                               <*> getMultilineText extext <! [("style","visibility:hidden"),("id","hiddenTextarea")]
                    <++ acedit
                    <** br
                    ++> submitButton "compile"
                    <++ br))) <! [("onsubmit","return copyContent()")]
      let name= strip name'
          hsfile = name ++ ".hs"
          code= filter (/='\r') $ T.unpack r
          des= unlines $ map (drop 2) . takeWhile ("--" `L.isPrefixOf`) $ lines code
      liftIO $ writeFile  (projects ++ hsfile) code
--      r <- liftIO . shell $ inDirectory projects $ genericRun "/app/.cabal/bin/hastec" [hsfile,"--output-html"] ""
--      r <- liftIO . shell $ inDirectory projects $ genericRun "/home/user/.cabal/bin/hastec" [hsfile,"--output-html"] ""
      r <- liftIO . shell $ inDirectory projects $ genericRun "hastec" [hsfile,"--output-html"] ""
      case r of
        Left errs -> fromStr ("*******Failure: not found hastec"++  errs) ++> empty
        Right (r,out,err) ->
          case r of
              True  -> do
                 Examples exampleList <- liftIO $ atomically $ readDBRef examples
                      `onNothing` error "examples empty"
                 liftIO . atomically . writeDBRef examples . Examples . L.nub $ (Example (name++".hs") des):exampleList
                 return name
              False -> fromStr err ++> empty
    page $ (a  ! href  (fromString("/"++name++".html")) $ "execute") ++> empty


strip name'=
  let rname= reverse name'
  in  if "sh." `L.isPrefixOf` rname then reverse $ drop 3 rname else name'

handle :: Example -> View Html IO String
handle e= do
 let name'= exname e
     name = strip $ exname e
     html = projects ++ name ++ ".html"

 (wlink name' << name' <++ br) `wcallback` const (do
      exist <- liftIO $ doesFileExist html
      wraw $ do
           b  << name'
           ": "
           toHtml $ desc e 
      if exist
       then
        (a  ! href  (fromString("/"++name++".html")) $ "execute") ++> empty
        <|> wlink name'  " modify" <++ br
       else return name')


acedit = [shamlet|
 <style type="text/css" media="screen">
    #editor {
        position: relative;
        width: 100%;
        height: 200;
    }


 <div id="editor">function foo(items) {
    var x = "All this is syntax highlighted";
    return x;
  }


 <script src="//cdnjs.cloudflare.com/ajax/libs/ace/1.1.3/ace.js" type="text/javascript" charset="utf-8">
 <script>
    copyContent1();
    var editor = ace.edit("editor");
    // editor.setTheme("ace/theme/monokai");
    editor.getSession().setMode("ace/mode/haskell");

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
