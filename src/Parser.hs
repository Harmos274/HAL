module Parser
    ( parse,
      Expression (..),
    ) where

import Control.Exception (throw)

import Lexer (Token (..))
import Exception (HExceptions (ParsingException))

data Expression = Atom String | Quoted Expression | Seq [Expression] deriving Show

parse :: [Token] -> [Expression]
parse []                   = []
parse (Lexer.Comment _:xs) = parse xs
parse tokens               = (parse' . parseExpression) tokens

parse' :: (Expression, [Token]) -> [Expression]
parse' (expr, tokens) = expr:parse tokens

parseExpression :: [Token] -> (Expression, [Token])
parseExpression (OpenParen:xs) = collectSeq xs
parseExpression (Word word:xs) = (Atom word, xs)
parseExpression (Quote    :xs) = mapFst Quoted $ parseExpression xs
parseExpression (Comment _:xs) = parseExpression xs
parseExpression (CloseParen:_) = throw $ ParsingException "Unmatched closing parenthesis"
parseExpression []             = throw $ ParsingException "Empty expression"

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst func (lhs, rhs) = (func lhs, rhs)

collectSeq :: [Token] -> (Expression, [Token])
collectSeq tokens = collectSeq' ([], tokens)

collectSeq' :: ([Expression], [Token]) -> (Expression, [Token])
collectSeq' (ret, CloseParen:xs) = (Seq $ reverse ret, xs)
collectSeq' (ret, Comment _ :xs) = collectSeq' (ret, xs)
collectSeq' (ret, Word word :xs) = collectSeq' (Atom word:ret, xs)
collectSeq' (ret, OpenParen :xs) = collectSeq' $ collectSeq'' ret $ collectSeq xs
collectSeq' (ret, Quote     :xs) = collectSeq' $ collectSeq'' ret $ mapFst Quoted $ parseExpression xs
collectSeq' (_, [])              = throw $ ParsingException "Unmatched opening parenthesis"

collectSeq'' :: [Expression] -> (Expression, [Token]) -> ([Expression], [Token])
collectSeq'' list (expr, tokens) = (expr:list, tokens)
