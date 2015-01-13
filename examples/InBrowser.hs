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

instance Alternative InBrowser where
  g <|> f =  let InBrowser s x = g
                 InBrowser s' x'= f 
             in  InBrowser (s ++ " <|> " ++ s') $ x

wlink x s=  InBrowser ("wlink "++ show x++" " ++show s) x   

inBrowser (InBrowser s x)= s

main= do
  print $ inBrowser $ do x <- return 3 ; wlink x x
  print $ inBrowser $ wlink 2 3 <|> wlink 3 4


