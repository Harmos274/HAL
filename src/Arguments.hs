module Arguments
    ( parseArgs,
    ) where

import Control.Exception (throw, catch, IOException)
import Control.Monad (foldM)

import Exception (HExceptions (ArgumentException))

type FileContent = String
data Argument = Computable FileContent
              | Interpretable FileContent
              | NoArgument deriving (Show)

parseArgs :: [String] -> IO Argument
parseArgs [] = throw $ ArgumentException "hal should be used with at least one argument."
parseArgs s  = catch (foldM argInterpreter NoArgument s) (\e -> throw $ ArgumentException $ show (e::IOException))

argInterpreter :: Argument -> String -> IO Argument
argInterpreter NoArgument         "-i" = return $ Interpretable ""
argInterpreter (Computable fc)    "-i" = return $ Interpretable fc
argInterpreter (Interpretable _)  "-i" = throw $ ArgumentException "Invalid argument, please use \"-h\""
argInterpreter NoArgument         s    = readFile s >>= (return . Computable)
argInterpreter (Computable fc)    s    = readFile s >>= (return . Computable . (++ fc))
argInterpreter (Interpretable fc) s    = readFile s >>= (return . Interpretable . (++ fc))