-- Haste calling JScript calling Haskell
-- based on https://github.com/coopernurse/haste-james/blob/bd200d3a0a0b81f83bc56c6b7053bf1769ec5088/ffi-export/export.hs

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import Haste.Foreign
import Haste.Perch

addInt :: Int -> Int -> IO Int
addInt x y = return $ x + y

addIntAsync :: Int -> Int -> Opaque (Int -> IO ()) -> IO ()
addIntAsync x y callback = setTimeout 0 add
	where add = addInt x y >>= fromOpaque callback

main = do
    body <- getBody
    export "addInt" addInt
    export "addIntAsync" addIntAsync
    r <- evalFormula "Haste.addInt(2, 2)"
    build (pre r) body


evalFormula :: String  ->  IO Double
evalFormula=  ffi $  
      "(function(exp){return eval(exp)})"
