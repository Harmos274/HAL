module Lexer
    ( lexer,
      Token (..),
    ) where

import Control.Exception (throw)
import Data.Char (isSpace)

import Exception (HExceptions (LexerException))

data Token = OpenParen | CloseParen | Word String | Comment String  deriving Show

lexer :: String -> [Token]
lexer []                   = []
lexer ('(':xs)             = OpenParen  : lexer xs
lexer (')':xs)             = CloseParen : lexer xs
lexer ('"':xs)             = collectStringLitteral xs
lexer (';':xs)             = collectComment xs
lexer s@(l:xs) | isSpace l = lexer xs
               | otherwise = collectWord s

collectComment :: String -> [Token]
collectComment = collectComment' . break (== '\n')

collectComment' :: (String, String) -> [Token]
collectComment' (com, xs) = Comment com : lexer xs

collectWord :: String -> [Token]
collectWord = collectWord' . break isDelimiter

collectWord' :: (String, String) -> [Token]
collectWord' (w, xs) = Word w : lexer xs

isDelimiter :: Char -> Bool
isDelimiter '"' = True
isDelimiter '(' = True
isDelimiter ')' = True
isDelimiter ';' = True
isDelimiter chr = isSpace chr

collectStringLitteral :: String -> [Token]
collectStringLitteral list = collectWord' $ collectStringLitteral' ([], list)

collectStringLitteral' :: (String, String) -> (String, String)
collectStringLitteral' (_  ,          []) = throw $ LexerException  "Unmatched quote"
collectStringLitteral' (ret, '\\':chr:xs) = collectStringLitteral'  (chr:'\\':ret, xs)
collectStringLitteral' (ret,      '"':xs) = collectStringLitteral'' (ret, xs)
collectStringLitteral' (ret,      chr:xs) = collectStringLitteral'  (chr:ret, xs)

collectStringLitteral'' :: (String, String) -> (String, String)
collectStringLitteral'' (lhs, rhs) = (reverse lhs, rhs)
