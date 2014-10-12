-- Example of spreadsheet-like behaviour using loeb iteration
-- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
-- with loop detection and resolution.
--
-- This program calculates speed, time and space. Each one depends on the other two
-- Each cell has two values: his current entered value and the expression which 
-- calculates it from other cell values/expressions. curcularity is permitted.
--
-- The Cell recalculation code uses the famous loeb expression (see the link below)
-- But to speed up a little, in this case (Map String Float) is used instead of a list of floats
-- Since loeb enters in a infinite loop when circular expressions are used, the
-- program counts the loops and reduces complexity by progressively substituting
-- formulas by cell values until the expression has no loops
--
-- This program is configured for inmediate recalculation, but that can be changed
-- to allow the modification of more than one cell be

{-# LANGUAGE   TypeSynonymInstances
             , FlexibleInstances
             , OverloadedStrings #-}
import Haste.Foreign
import Haste
import Haste.HPlay.View hiding (head)
import Haste.HPlay.Cell as Cell
import Control.Applicative
import Data.Monoid
import qualified Data.Map as M hiding ((!))
import Control.Monad.IO.Class
import System.IO.Unsafe
import Data.IORef
import Debug.Trace
import Control.Exception
import Data.List
import Control.Monad
import Data.Maybe
(!>)= flip trace

main= runBody $   static $ do
   wraw $ h1 $ ("calculate space, time and speed " :: String)
   printw ("Can change one of the cell and the other two will be recalculated"::String)
   pre <<< ("a car runs for" ++> space
             **> " Kms during" ++> time **> " Hours;\n"
             ++> "His mean speed was" ++> speed <++ "Km/h\n") 
             
--   (input ! atr "type" "submit" ! atr "value" "calc" ) `pass` OnClick
   calc
   where
   space= mkscell "space" (Just 1) ( scell "speed" * scell "time")  ! size "5"
   time = mkscell "time"  (Just 1) (scell "space" / scell "speed") ! size "5"
   speed= mkscell "speed" (Just 1) (scell "space" / scell "time")  ! size "5"
   
   size= atr "size"

-- <<< is the operator for enclosing a widget within a tag
--     All the spreadsheet expression is enclosed within a 'pre' tag

-- ++> HPlay operator for adding HTML to a widget

-- **> HPlay operator to discard the output of the left widget and compose with the right one
--     while presenting both left and right rendering composed

-- The recursive Cell calculation DSL BELOW ------
-- This code will probably be addded to Haste.HPlay.Cell

-- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
-- loeb ::  Functor f => f (t -> a) -> f a
loeb :: M.Map String (Expr a) -> M.Map String a
loeb x = fmap (\a -> a (loeb  x)) x

-- cell ::  Num a => String -> M.Map String a -> a
scell n= \vars -> case M.lookup n vars of
    Just exp -> inc n  exp 
    Nothing -> error $ "cell error in: "++n
  where
  inc n exp= unsafePerformIO $ do
     tries <- readIORef rtries
     if tries <= maxtries 
       then  do
          writeIORef rtries  (tries+1)
          return exp
             
       else  error n 

circular n= "loop detected in cell: "++ n  ++ " please fix the error"

printw :: ToElem a => a -> Widget ()
printw = wraw . pre

type Expr a = M.Map String a -> a

rtries= unsafePerformIO $ newIORef $ (0::Int)
maxtries=  3* (M.size $ unsafePerformIO $ readIORef rexprs)

rexprs :: IORef (M.Map String (Expr Float))
rexprs= unsafePerformIO $ newIORef M.empty

rmodified :: IORef (M.Map String (Expr Float))
rmodified= unsafePerformIO $ newIORef M.empty

mkscell :: String -> Maybe Float -> Expr Float -> Widget ()
mkscell name val expr=  static $ do
   liftIO $ do
     exprs <- readIORef rexprs
     writeIORef rexprs $ M.insert name expr exprs
     return exprs
   r <- mk (boxCell name) val `fire` OnChange
   liftIO $ do
        mod <- readIORef rmodified !> name 
        writeIORef rmodified  $ M.insert  name (const r)  mod 
 `continuePerch`  name
 
   


continuePerch :: Widget a -> ElemID -> Widget a
continuePerch w eid= View $ do
  FormElm f mx <- runView w
  return $ FormElm (c f) mx
  where
  c f =Perch $ \e' ->  do
     build f e'
     elemid eid
     
  elemid id= elemById id >>= return . fromJust


calc :: Widget ()
calc= do
  nvs <- liftIO $ readIORef rmodified
  when (not $ M.null nvs) $ do
    values <-liftIO $ handle doit calc1
    mapM_ (\(n,v) -> boxCell n .= v)  values 
  liftIO $ writeIORef rmodified M.empty
  where
  calc1  :: IO [(String,Float)]
  calc1=do 
    writeIORef rtries 0
    cells <- liftIO $ readIORef rexprs
    nvs   <- liftIO $ readIORef rmodified
    let mvalues = M.union nvs  cells 
        evalues = loeb mvalues 
        
    toStrict $ M.toList evalues 
  
  toStrict xs = print xs >> return xs
  
  doit :: SomeException -> IO [(String,Float)]
  doit e= do
    nvs <- readIORef rmodified
    exprs <- readIORef rexprs
    case  M.keys exprs \\ M.keys nvs of
      [] -> do
         let Just (ErrorCall n)= fromException e
         let err= circular n
         alert err
         error err
      (name:_) -> do
         mv <- getter $ boxCell name
         case mv of
            Nothing -> return []
            Just v -> do
                writeIORef rmodified  $ M.insert name (const v) nvs
                calc1

instance Show (Expr a) 

instance Eq (Expr a) 

instance (Num a,Eq a,Fractional a) =>Fractional (x -> a)where
     f / g = \x -> f x / g x


instance (Num a,Eq a) => Num (x -> a) where
     fromInteger = const . fromInteger
     f + g = \x -> f x + g x
     f * g = \x -> f x * g x
     negate = (negate .)
     abs = (abs .)
     signum = (signum .)