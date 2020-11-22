module Arguments
    ( parseArgs,
      SourceCode (..),
      IsInterractive (..),
    ) where

import Control.Exception (throw)

import Exception (HExceptions (ArgumentException))

type FileContent = String
newtype IsInterractive = IsInterractive Bool deriving (Show)
data SourceCode = SourceCode IsInterractive [FileContent] | SOS

instance Show SourceCode where
    show SOS                   = "USAGE: ./hal file [...] [-i]\n\tfile\tA Lisp file\t-i\tOpen interpreter"
    show (SourceCode inter fc) = "SourceCode " ++ show inter ++ " " ++ show fc

interactive :: IsInterractive
interactive = IsInterractive True

computed :: IsInterractive
computed = IsInterractive False

parseArgs :: [String] -> SourceCode
parseArgs []         = throw $ ArgumentException "hal should be used with at least one argument."
parseArgs ["-h"]     = SOS
parseArgs ["--help"] = SOS
parseArgs s          = parseArgs' $ reverse s

parseArgs' :: [String] -> SourceCode
parseArgs' ("-i":xs) = SourceCode interactive $ reverse xs
parseArgs' s         = SourceCode computed    $ reverse s