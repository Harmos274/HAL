module Main where

-- System
import Control.Exception (handle)
import System.Environment (getArgs)
--

import Exception (exceptionHandler)
import Arguments (parseArgs, SourceCode (..), IsInterractive (..))
import Lexer (lexer)
import Parser (parse)

main :: IO ()
main = handle exceptionHandler $ getArgs >>= (halgo . parseArgs)

halgo :: SourceCode -> IO ()
halgo SOS                                    = print SOS
halgo (SourceCode (IsInterractive True ) fc) = mapM (fmap lexer . readFile) fc >>= print
halgo (SourceCode (IsInterractive False) fc) = print "computable"
