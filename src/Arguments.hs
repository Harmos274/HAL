module Arguments
    ( parseArgs,
    ) where

import Control.Exception (throw)
import Data.Char (isDigit, isSpace)

import Exception (HExceptions (LexerException))

newtype FileContent = FileContent String deriving (Show)

data Argument = Computable FileContent
              | Interpretable FileContent deriving (Show)

parseArgs :: [String] -> Argument
parseArgs s = Computable $ FileContent $ concat s

toto :: Argument -> String -> Argument
toto (Computable fc) "-i" = Interpretable fc
toto (Computable fc) s    = Computable fc
