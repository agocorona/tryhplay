-- Hplayground Ajax control is de-inverted. This means that ajax request follows the
-- normal flow. there is no need to se hup a response handler for each request
-- ajax is just a monadic statement in the Widget monad.
--
-- This example make use of the web service por compilation of tryplayground and
-- show the result
--
-- The service in the server is "compileServ" at 
-- https://github.com/agocorona/tryhplay/blob/master/Main.hs
--
import Haste.HPlay.View

main= runBody $ do
     r <- ajax POST "/compile" [("name","test"),("text","main=print \"hello\"")]
     wprint (r :: Maybe String)