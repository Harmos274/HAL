module Main where

-- System
import Control.Exception (handle)
import System.Environment (getArgs)
--

import Exception (exceptionHandler)
import Arguments (parseArgs, SourceCode (..), IsInterractive (..))
import Lexer (lexer)
import Parser (parse)
import Evaluator (evaluate, Value)

main :: IO ()
main = handle exceptionHandler $ getArgs >>= (halgo . parseArgs)

halgo :: SourceCode -> IO ()
halgo SOS                                    = print SOS
halgo (SourceCode (IsInterractive True ) fc) = mapM (fmap lexer . readFile) fc >>= print
halgo (SourceCode (IsInterractive False) fc) = mapM (fmap (evaluate . parse . lexer) . readFile) fc >>= (toto . concat)

toto :: [Value] -> IO ()
toto = foldr ((>>) . print) (return ())