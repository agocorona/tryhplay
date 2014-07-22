{-# LANGUAGE  DeriveDataTypeable #-}
import Haste.HPlay.View
import Haste.HPlay.Cell
import Haste.Perch
import Haste
import Haste.LocalStorage
import Haste.DOM
import Control.Applicative
import Prelude hiding (div,span,id,all)
import Data.Typeable
import Control.Monad(when)
import Control.Monad.IO.Class
import qualified Data.IntMap as M
import Data.Monoid
import Data.Maybe


main= do
--   hs <- elemsByTagName "head"

--   build (link ! atr "rel" "stylesheet"
--               ! href "bower_components/todomvc-common/base.css")  $ Prelude.head hs

   withElem "idelem" .  runWidget $ todo

data Status= Completed | Active deriving (Typeable, Show,Eq,Read)
type Tasks = M.IntMap  (String,Status)

data TaskAction =  Done | Destroy

data PresentationMode= Mode String deriving Typeable

all= ""
active= "active"
completed= "completed"



todo ::  Widget ()
todo = do
      section ! id "todoapp" $ do
          nelem "header" ! id "header"
          section ! id "main" $ do
            ul ! id "todo-list"  $ noHtml
          footer ! id "footer"  $ do
            span ! id "todo-count" $ noHtml
            ul ! id "filters" $ noHtml
            span ! id "clear-holder" $ noHtml
      footer ! id "info" $ do
            p "Double-click to edit a todo"
            p $ do
               toElem "Created by "
               a ! href "http://twitter.com/agocorona" $ "Alberto G. Corona"
               p $ do
                  toElem "Part of "
                  a ! href "http://todomvc.com" $ "TodoMVC"
     ++> header
      *> toggleAll
      *> filters all
      *> numActive
      *> clearCompleted

 where

 numActive= at "todo-count" Insert $ do
    n <- getTasks >>= return . M.size . M.filter ((==) Active . snd)
    wraw $ case n of
      1 -> do
                strong "1"
                toElem " item left"
      n -> do
                strong (show n)
                toElem " items left"

 clearCompleted= at "clear-holder" Insert $ do
    tasks <- getTasks
    let n =  M.size . M.filter ((==) Completed . snd)   $ tasks
    when (n >0) $ do
        resetEventData
        (button ! id "clear-completed" $ "Clear completed") `pass` OnClick

        setTasks $ M.filter ((==) Active . snd) tasks
        displayFiltered



 filters op =at "filters" Insert $ filters' op

    where
    filters' op= (links op `wake` OnClick)
      `wcallback` (\op' -> do
        liftIO $ print op'
        setSData $ Mode op'
        displayFiltered **> return ()
        filters' op')

    links op=
        li ! clas op all       <<< wlink all  (toElem "All")    <|>
        li ! clas op active    <<< wlink active  (toElem "Active")  <|>
        li ! clas op completed <<< wlink completed  (toElem "Completed")

    clas current op= atr "class" $ if current== op then "selected" else "unsel"

 header = at "header" Insert $ h1 "todos" ++> newTodo

 toggleAll = at "main" Prepend $ do
    CheckBoxes t <- setCheckBox False "toggle" `wake` OnClick ! atr "class" "toggle-all"
    let newst=  case t of
            [] ->  Active
            _  ->  Completed
    tasks <- getTasks
    filtered  <- getFiltered tasks
    let filtered' = M.map (\(t,_) -> (t,newst)) filtered
        tasks'    = M.union filtered' tasks
    setTasks tasks'
    displayFiltered
    numActive

 displayFiltered = (do
    tasks <- getTasks
    filtered <- getFiltered tasks
    at "todo-list" Insert $ foldl (<|>) empty $ reverse
      [display  tasks t  | t <- M.assocs filtered])
   **> return ()

 getFiltered tasks= do
   Mode todisplay <- getSData <|> return (Mode all)
   return $ M.filter (fil todisplay)  tasks
   where
   fil  f (t,st)
     | f==all=   True
     | f==active = case st of
                         Active -> True
                         _      -> False
     | otherwise = case st of
                         Completed -> True
                         _         -> False


 newTodo= do
      let entry= boxCell "new-todo"
      task <- mk entry Nothing `wake` OnKeyUp
                ! atr "placeholder" "What needs to be done?"
                ! atr "autofocus" ""
      EventData evname evdata <- getEventData
      when( evdata == Key 13) $ do
         entry .= ""
         tasks <- getTasks
         let i= M.size tasks
             tasks'= M.insert  i (task,Active) tasks
         setTasks tasks'
         numActive
         Mode m <- getSData <|> return (Mode all)
         when (m /= completed) $ at "todo-list" Prepend $ display tasks' (i,(task,Active))
         return ()


 display tasks (i,(task,st)) =
   (li <<< ( do
    let checked= case st of Completed -> True; Active -> False
    CheckBoxes ch <- setCheckBox checked "check" `wake` OnClick ! atr "class" "toggle"
    case ch of
        ["check"] -> changeState tasks i Completed task >> viewEdit Completed task
        _         -> changeState tasks i Active task    >> viewEdit Active task



   <** do CheckBoxes r <- setCheckBox False "destroy" `wake` OnClick  ! atr "class" "destroy"
          continueIf (not $ null r) ()))
  `wcallback` (const $ setTasks $ M.delete i tasks)

   where
   changeState tasks i stat task= do
        setTasks $ M.insert  i (task,stat) tasks
        clearCompleted

   viewEdit st task = do
     let lab= case st of
            Completed  -> label ! atr "class" "completed"
            Active     -> label

     lab  task `pass` OnDblClick

    `wcallback` const (do
            ntask <-  inputString (Just task) `wake` OnKeyUp ! atr "class" "edit"
            EventData _ (Key k) <- getEventData
            continueIf (k== 13) ntask

       `wcallback` viewEdit st)



getTasks= ((liftIO $ getItem "tasks") >>= \r ->
                 case r of
                     Right s -> return (read s)
                     Left s  -> empty)
           <|> return (M.empty) :: Widget Tasks

setTasks :: MonadIO m => Tasks -> m ()
setTasks =  liftIO . setItem "tasks" . show
