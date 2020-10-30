module Main where

-- System
import Control.Exception (handle, throw)
import System.Environment (getArgs)
--

import Exception (exceptionHandler)
import Arguments (parseArgs)

main :: IO ()
main = handle exceptionHandler $ getArgs >>= (print . parseArgs)
