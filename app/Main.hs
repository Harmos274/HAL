module Main where

-- System
import Control.Exception (handle)
import System.Environment (getArgs)
--

import Exception (exceptionHandler)
import Arguments (parseArgs, SourceCode (..), IsInterractive (..))

main :: IO ()
main = handle exceptionHandler $ getArgs >>= parseArgs >>= halgo

halgo :: SourceCode -> IO ()
halgo (SourceCode (IsInterractive True ) fc) = print "interractive"
halgo (SourceCode (IsInterractive False) fc) = print "computable"