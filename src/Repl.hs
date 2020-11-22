module Repl
    ( repl
    ) where

import System.Console.Haskeline (runInputT, InputT, getInputLine, outputStrLn, handle, Settings (Settings),
                                 historyFile, complete, autoAddHistory,
                                 Completion, simpleCompletion, completeWord)

import qualified Data.Map.Strict as Map (keys)

import Exception (HExceptions (..))
import Lexer (lexer)
import Parser (parse)
import Evaluator (evaluateRepl, Context, Value)
import Data.List (isPrefixOf)

searchFunc :: Context -> String -> [Completion]
searchFunc ctx str = map simpleCompletion $ filter (str `isPrefixOf`) (Map.keys ctx)

replSettings :: Context -> Settings IO
replSettings ctx = Settings { historyFile    = Just "repl.history"
                            , complete       = completeWord Nothing " \t" $ return . searchFunc ctx
                            , autoAddHistory = True
                            }

repl :: Context -> IO ()
repl ctx = runInputT (replSettings ctx) (repl' ctx)

repl' :: Context -> InputT IO ()
repl' ctx = getInputLine "> " >>= repl'' ctx

repl'' :: Context -> Maybe String -> InputT IO ()
repl'' _   Nothing       = return ()
repl'' _   (Just "quit") = return ()
repl'' ctx (Just "")     = repl' ctx
repl'' ctx (Just input)  = handle (replExceptionHandler ctx) $ repl''' $ evaluateRepl ctx $ parse $ lexer input

repl''' :: (Context, [Value]) -> InputT IO ()
repl''' (ctx, val) = mapM_ (outputStrLn . show) val >> repl' ctx

replExceptionHandler :: Context -> HExceptions -> InputT IO ()
replExceptionHandler fc (ArgumentException s)    = outputStrLn ("Argument exception : "   ++ s) >> repl' fc
replExceptionHandler fc (LexingException s)      = outputStrLn ("Lexing exception : "     ++ s) >> repl' fc
replExceptionHandler fc (ParsingException s)     = outputStrLn ("Parser exception : "     ++ s) >> repl' fc
replExceptionHandler fc (EvaluationException s)  = outputStrLn ("Evaluation exception : " ++ s) >> repl' fc
