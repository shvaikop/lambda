{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
module MyParser
    ( VarId
    , ExprName
    , Statement(..)
    , LamExpr(..)
    , parseEntry
    ) where

import Text.Parsec
    ( char, letter, spaces, many1, (<|>), parse, try, ParseError )
import Text.Parsec.String ( Parser )
import Data.List ((\\))
import Data.Map (Map, empty, lookup, member, (!), insert)

import MyTypes

-- parses a char, skipping spaces before and after
symChar :: Char -> Parser Char
symChar c = spaces *> char c <* spaces

symLetter :: Parser Char
symLetter = spaces *> letter <* spaces

-- pubic function that main will call to parse
parseEntry :: String -> Either ParseError Statement
parseEntry arg = parse parseTop "error" arg

parseTop :: Parser Statement
parseTop = try statementAssignParse <|> statementExprParse

-- <name> = <expresssion>
statementAssignParse :: Parser Statement
statementAssignParse = do
    name <- many1 letter
    symChar '='
    expr <- lamExprParse
    return (Assign name expr)

-- <expression>
statementExprParse :: Parser Statement
statementExprParse = do
    expr <- lamExprParse
    return (Expr expr)

lamExprParse :: Parser LamExpr
lamExprParse = do
  exprs <- many1 (spaces *> (parenParse <|> varParse  <|> abstrParse <|> applParse) <* spaces)
  return (foldl1 Appl exprs)

-- parses a local (single char) variable or an expression name ($...)
varParse :: Parser LamExpr
varParse = localVarParse <|> exprSubstParse

-- parses a variable made up of a single character
localVarParse :: Parser LamExpr
localVarParse = do
    var <- symLetter
    return (Var [var])

-- parses a variable name that is a name for a named expression
exprSubstParse :: Parser LamExpr
exprSubstParse = do
    spaces
    char '$'
    var <- many1 letter
    spaces
    return (ExprSubst var)

-- parses abstractions, single char variable names for now
abstrParse :: Parser LamExpr
abstrParse = do
    char '\\'
    var <- letter
    char '.'
    body <- lamExprParse
    return (Abstr [var] body)

-- parses parentheses and returns expression inside them
parenParse :: Parser LamExpr
parenParse = do
    symChar '('
    expr <- lamExprParse
    symChar ')'
    return expr

applParse :: Parser LamExpr
applParse = do
    appls <- many1 (varParse <|> parenParse)
    return (foldl1 Appl appls)
