-- The hello world program using hplayground

import Haste.HPlay.View
import Control.Applicative

main= runBody $  fromStr "hello world" ++> empty