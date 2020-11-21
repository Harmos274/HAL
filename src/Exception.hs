module Exception
    ( HExceptions (..),
      exceptionHandler
    ) where

import Control.Exception (Exception)
import Epitech.ReturnType (failure)

data HExceptions = ArgumentException String
                 | LexingException String
                 | ParsingException String
                 | EvaluationException String
                 deriving (Show)

instance Exception HExceptions

exceptionHandler :: HExceptions -> IO ()
exceptionHandler (ArgumentException s)   = putStrLn ("Argument exception : "   ++ s) >> failure
exceptionHandler (LexingException s)     = putStrLn ("Lexing exception : "     ++ s) >> failure
exceptionHandler (ParsingException s)    = putStrLn ("Parsing exception : "    ++ s) >> failure
exceptionHandler (EvaluationException s) = putStrLn ("Evaluation exception : " ++ s) >> failure
