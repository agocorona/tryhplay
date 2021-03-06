import Control.Applicative

data InBrowser a= InBrowser String a deriving Show

instance Functor InBrowser where
   fmap f (InBrowser s x)= InBrowser s  $ f x

instance Applicative InBrowser where
   pure = InBrowser ""
   g <*> f= let InBrowser s x = g
                InBrowser s' x'= f 
            in  InBrowser (s ++ s') $ x x' 
   
instance Monad InBrowser where
   g >>= f =  let InBrowser s x = g
                  InBrowser s' x'= f x
              in  InBrowser (s ++ s') x' 
   return x= InBrowser "" x

wlink x s=  InBrowser ("wlink "++ show x++" " ++show s) x   

extract (InBrowser s x)= s

main= putStrLn $ extract $ return 3 >>= \y -> wlink y y

