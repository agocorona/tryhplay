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

{-# LANGUAGE OverloadedStrings #-}

import Haste.HPlay.View
import Haste.HPlay.Cell

main= runBody $   static $ do
   wraw $ h1 $ ("calculate space, time and speed " :: String)
   wprint ("Can change one of the cell and the other two will be recalculated"::String)
   pre <<< ("a car runs for" ++> space
             **> " Kms during" ++> time **> " Hours;\n"
             ++> "His mean speed was" ++> speed <++ "Km/h\n") 
             
--   (input ! atr "type" "submit" ! atr "value" "calc" ) `pass` OnClick
   calc
   where
   space= mkscell "space" (Just 1) (gcell "speed" * gcell "time")  ! size "5" 
   time = mkscell "time"  (Just 1) (gcell "space" / gcell "speed") ! size "5" 
   speed= mkscell "speed" (Just 1) (gcell "space" / gcell "time")  ! size "5" 
   
   size= atr "size"

-- <<< is the operator for enclosing a widget within a tag
--     All the spreadsheet expression is enclosed within a 'pre' tag

-- ++> HPlay operator for adding HTML to a widget

-- **> HPlay operator to discard the output of the left widget and compose with the right one
--     while presenting both left and right rendering composed

-- The recursive Cell calculation DSL BELOW ------
-- This code will probably be addded to Haste.HPlay.Cell

