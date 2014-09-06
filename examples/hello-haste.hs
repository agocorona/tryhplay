-- This program will ask the user for her name, and echo a greeting back into
-- this text box as a comment.
-- NOTE: Haste programs with server side do not run in this IDE

import Haste
import Haste.Foreign
import Haste.Prim

main = do
  body <- getBody 
  e <- newElem "div"
  name <- prompt "Dearest user, please tell me your name"
  text <- newTextElem  $ "Hello " ++ name
  addChild  text e
  addChild e body

getBody= ffi $ toJSStr "(function(){return document.body;})"
