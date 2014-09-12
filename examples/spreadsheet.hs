-- Example of spreadsheet-like behaviour using loeb iteration
-- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
-- with loop detection and resolution.


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
--(!>)= flip trace

main= runBody $ do
   printw ("calculate space, time and speed " :: String)
   printw ("Can change one of the cell and the other two will be recalculated"::String)
   pre <<< ("a car runs for" ++> space
             **> " Kms\nduring" ++> time **> " Hours;\n"
             ++> "His mean speed was" ++> speed <++ "Km/h")
   calc
   where
   space= mkcell "space" (Just 1) (cell "speed" * cell "time")  ! size "5"
   time = mkcell "time"  (Just 3) (cell "space" / cell "speed") ! size "5"
   speed= mkcell "speed" (Just 3) (cell "space" / cell "time")  ! size "5"
   
   size= atr "size"
   
-- ++> HPlay operator for adding HTML to a widget
-- **> HPlay operator to discard the output of the left widget and compose with the right one
--     while presenting both left and right rendering composed

-- The recursive Cell DSL BELOW ------
-- This code will probably be addded to Haste.HPlay.Cell

-- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
-- loeb ::  Functor f => f (t -> a) -> f a
loeb :: M.Map String (Expr a) -> M.Map String a
loeb x = fmap (\a -> a (loeb  x)) x

-- cell ::  Num a => String -> M.Map String a -> a
cell n= \vars -> case M.lookup n vars of
    Just exp -> inc n  exp 
    Nothing -> error $ "cell error in: "++n
  where
  inc n exp= unsafePerformIO $ do
     tries <- readIORef rtries
     if tries <= maxtries 
       then  do
          writeIORef rtries  (tries+1)
          return exp
             
       else  do
          let err= "loop detected in cell: "++ n 
                   ++ " please fix the error"
          -- alert err
          error err 


printw :: ToElem a => a -> Widget ()
printw = wraw . pre

type Expr a = M.Map String a -> a

rtries= unsafePerformIO $ newIORef $ (0::Int)
maxtries=  3* (M.size $ unsafePerformIO $ readIORef rexprs)

rexprs :: IORef (M.Map String (Expr Float))
rexprs= unsafePerformIO $ newIORef M.empty

rmodified :: IORef (M.Map String (Expr Float))
rmodified= unsafePerformIO $ newIORef M.empty

mkcell :: String -> Maybe Float -> Expr Float -> Widget ()
mkcell name val expr=  do
   liftIO $ do
     exprs <- readIORef rexprs
     writeIORef rexprs $ M.insert name expr exprs
   r <- mk (boxCell name) val  `fire` OnChange 
   liftIO $ writeIORef rmodified  $ M.singleton  name $ const r 

calc :: Widget ()
calc= do
  values <-liftIO $ handle doit calc1

  mapM_ (\(n,v) -> boxCell n .= v)  values 
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
  doit _= do
    nvs <- readIORef rmodified
    exprs <- readIORef rexprs
    let name= head $ M.keys exprs \\ M.keys nvs
    v <- get $ boxCell name
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