module Arguments
    ( parseArgs,
      SourceCode (..),
      IsInterractive (..),
    ) where

import Control.Exception (throw, catch, IOException)

import Exception (HExceptions (ArgumentException, SendHelp))

type FileContent = String
newtype IsInterractive = IsInterractive Bool deriving (Show)
data SourceCode = SourceCode IsInterractive [FileContent] deriving (Show)

interactive :: IsInterractive
interactive = IsInterractive True

computed :: IsInterractive
computed = IsInterractive False

parseArgs :: [String] -> IO SourceCode
parseArgs []           = throw $ ArgumentException "hal should be used with at least one argument."
parseArgs ("-h":_)     = throw SendHelp
parseArgs ("--help":_) = throw SendHelp
parseArgs s            = catch (parseArgs' $ reverse s) (\e -> throw $ ArgumentException $ show (e::IOException))

parseArgs' :: [String] -> IO SourceCode
parseArgs' ("-i":xs) = SourceCode interactive <$> mapM readFile xs
parseArgs' s         = SourceCode computed    <$> mapM readFile s