module Lexer
    ( lexer,
      Token(..),
    ) where

import Control.Exception (throw)
import Data.Char (isDigit, isSpace)

import Exception (HExceptions (LexerException))

data Token = OpenParen | CloseParen | Function String deriving (Show)

lexer :: String -> [Token]
lexer []                      = []
lexer ('(':xs)                = OpenParen : lexer xs
lexer (')':xs)                = CloseParen : lexer xs
lexer (a:xs) | isSpace a      = lexer xs
lexer str                     = (lexer' . getNumber) str

{-# INLINE lexer' #-}
lexer' :: (String, String) -> [Token]
lexer' (nbr, xs) = Function nbr : lexer xs

getNumber :: String -> (String, String)
getNumber = tupleIsAFloat . span isPartOfNumber

tupleIsAFloat :: (String, String) -> (String, String)
tupleIsAFloat ([], _)                    = throw $ LexerException "Invalid token"
tupleIsAFloat t@(a, _)  | isValidFloat a = t
                        | otherwise      = throw $ LexerException (a ++ " is not a float.")

isValidFloat :: String -> Bool
isValidFloat []       = True
isValidFloat ['.']    = False
isValidFloat ('.':xs) = isValidFloat' xs
isValidFloat (_:xs)   = isValidFloat xs

isValidFloat' :: String -> Bool
isValidFloat' []      = True
isValidFloat' ('.':_) = False
isValidFloat' (_:xs)  = isValidFloat' xs

isPartOfNumber :: Char -> Bool
isPartOfNumber '.' = True
isPartOfNumber x   = isDigit x

