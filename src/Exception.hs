module Exception
    ( HExceptions (..),
      exceptionHandler
    ) where

import Control.Exception (Exception)
import Epitech.ReturnType (failure)

data HExceptions = ArgumentException String
                | LexerException String
                | ParserException String
                | EvaluationException String
                deriving (Show)

instance Exception HExceptions

exceptionHandler :: HExceptions -> IO ()
exceptionHandler (ArgumentException s)    = putStrLn ("Argument exception : "   ++ s) >> failure
exceptionHandler (LexerException s)       = putStrLn ("Lexing exception : "     ++ s) >> failure
exceptionHandler (ParserException s)      = putStrLn ("Parser exception : "     ++ s) >> failure
exceptionHandler (EvaluationException s)  = putStrLn ("Evaluation exception : " ++ s) >> failure
