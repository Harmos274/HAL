module Main where

-- System
import Control.Exception (handle)
import System.Environment (getArgs)
--

import Exception (exceptionHandler)
import Arguments (parseArgs)

main :: IO ()
main = handle exceptionHandler $ getArgs >>= parseArgs >>= print
