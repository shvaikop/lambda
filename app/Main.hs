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
  exprs <- many1 (parenParse <|> applParse <|> abstrParse)
  return (foldl1 Appl exprs)

-- parses and returns a String made up of alphabetic characters
varParse :: Parser LamExpr
varParse = do 
    var <- letter
    -- traceM $ "varParse: " ++ [var]
    return (Var [var])

-- parses abstractions, single char variable names for now
abstrParse :: Parser LamExpr
abstrParse = do 
    -- traceM "entered abstrParse"
    char '\\' 
    var <- letter
    char '.'
    body <- lamExprParse
    -- traceM $ "abstrParse: " ++ [var]
    return (Abstr [var] body)

-- parses parentheses and returns expression inside them
parenParse :: Parser LamExpr
parenParse = do
    char '('
    -- traceM "entered parenParse"
    expr <- lamExprParse
    char ')'
    -- traceM ("parenParse " ++ show expr)
    return expr

applParse :: Parser LamExpr
applParse = do
  appls <- many1 (varParse <|> parenParse)
--   traceM ("applParse: " ++ show appls)
  return (foldl1 Appl appls)




main :: IO ()
main = putStrLn "Hello, Haskell!"

-----  TESTS  -----
-------------------------------------------------------------------
test1 = parse lamExprParse "error" "\\f.\\x.fx"

test2 = parse lamExprParse "error" "\\f.\\x.f(fx)"

test3 = parse lamExprParse "error" "((\\x.x)((\\y.y)(\\y.y)))"

test4 = parse lamExprParse "error" "(\\x.x)((\\y.y)(\\y.y))"
