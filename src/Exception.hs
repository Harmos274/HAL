module Exception
    ( HExceptions (..),
      exceptionHandler
    ) where

import Control.Exception (Exception, IOException (..))
import Epitech.ReturnType

data HExceptions = SendHelp
                | ArgumentException String
                | LexerException String
                | ParserException String
                | EvaluationException String
                deriving (Show)

instance Exception HExceptions

sendHelp :: IO ()
sendHelp = mapM_ putStrLn ["USAGE: ./hal file [...] [-i]\n",
                           "\tfile\tA Lisp file",
                           "\t-i\tOpen interpreter"]

exceptionHandler :: HExceptions -> IO ()
exceptionHandler SendHelp                 = sendHelp >> success
exceptionHandler (ArgumentException s)    = putStrLn ("Argument exception : " ++ s) >> failure
exceptionHandler (LexerException s)       = putStrLn ("Lexing exception : " ++ s)   >> failure
exceptionHandler (ParserException s)      = putStrLn ("Parser exception : " ++ s)   >> failure
exceptionHandler (EvaluationException s)  = putStrLn ("Evaluation exception : " ++ s)  >> failure
