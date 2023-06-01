{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where

import Text.Parsec
import Text.Parsec.String
import Debug.Trace

data LamExpr = Var String
             | Abstr String LamExpr
             | Appl LamExpr LamExpr
             deriving(Show, Eq)

lamExprParse :: Parser LamExpr
lamExprParse = do
  exprs <- many1 (spaces *> parenParse <|> spaces *> applParse <|> spaces *> abstrParse)
  return (foldl1 Appl exprs)

-- parses a variable made up of a single character
varParse :: Parser LamExpr
varParse = do 
    var <- letter
    traceM $ "varParse: " ++ [var]
    return (Var [var])

-- parses abstractions, single char variable names for now
abstrParse :: Parser LamExpr
abstrParse = do 
    traceM "entered abstrParse"
    char '\\' 
    var <- letter
    char '.'
    body <- lamExprParse
    -- spaces
    traceM $ "abstrParse: " ++ [var]
    return (Abstr [var] body)

-- parses parentheses and returns expression inside them
parenParse :: Parser LamExpr
parenParse = do
    spaces
    char '('
    spaces
    traceM "entered parenParse"
    expr <- lamExprParse
    spaces
    char ')'
    spaces
    traceM ("parenParse " ++ show expr)
    return expr

applParse :: Parser LamExpr
applParse = do
  appls <- many1 ( spaces *> (varParse <|> parenParse) <* spaces )
  spaces
  traceM ("applParse: " ++ show appls)
  return (foldl1 Appl appls)




main :: IO ()
main = putStrLn "Hello, Haskell!"

-----  TESTS  -----
-------------------------------------------------------------------
test1 = parse lamExprParse "error" "\\f.\\x.fx"

test2 = parse lamExprParse "error" "\\f.\\x.f(fx)"

test3 = parse lamExprParse "error" "((\\x.x)((\\y.y)(\\y.y)))"

test4 = parse lamExprParse "error" "(\\x.x)((\\y.y)(\\y.y))"

test5 = parse lamExprParse "error" "             (\\x.x) ((\\y.y  ) (  \\y.y)  )"
