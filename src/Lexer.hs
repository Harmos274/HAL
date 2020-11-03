module Lexer
    ( lexer,
      Token(..),
    ) where

import Control.Exception (throw)
import Data.Char (isSpace)

import Exception (HExceptions (LexerException))

data Token = OpenParen | CloseParen | Word String deriving Show

lexer :: [Char] -> [Token]
lexer []                     = []
lexer ('(':xs)               = OpenParen : lexer xs
lexer (')':xs)               = CloseParen : lexer xs
lexer (chr:xs) | isSpace chr = lexer xs
lexer ('"':xs)               = (lexer'. collectQuote) xs
lexer list                   = (lexer'. collectWord) list

lexer' :: ([Char], [Char]) -> [Token]
lexer' (word, xs) = Word word : lexer xs

collectWord :: [Char] -> ([Char], [Char])
collectWord = break isSpace

collectQuote :: [Char] -> ([Char], [Char])
collectQuote list = collectQuote' ([], list)

collectQuote' :: ([Char], [Char]) -> ([Char], [Char])
collectQuote' (_, [])            = throw $ LexerException "Unmatched quote"
collectQuote' (ret, '\\':chr:xs) = collectQuote' (chr:'\\':ret, xs)
collectQuote' (ret, '"':xs)      = collectQuote'' (ret, xs)
collectQuote' (ret, chr:xs)      = collectQuote' (chr:ret, xs)

collectQuote'' :: ([Char], [Char]) -> ([Char], [Char])
collectQuote'' (lhs, rhs) = (reverse lhs, rhs)
