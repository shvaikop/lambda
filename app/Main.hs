{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where

import Text.Parsec
import Text.Parsec.String
import Debug.Trace
import System.IO
-- data LamAssignment = Assign String LamExpr
--     deriving (Show, Eq)

data LamExpr = Var String
             | Abstr String LamExpr
             | Appl LamExpr LamExpr     
             | Assign String LamExpr    -- assignment 
             deriving(Show, Eq)

parseTop :: Parser LamExpr
parseTop = lamAssignmentParse <|> lamExprParse

lamAssignmentParse :: Parser LamExpr
lamAssignmentParse = do
    name <- (many1 letter) <* spaces
    char '='
    spaces
    expr <- lamExprParse
    return (Assign name expr)

lamExprParse :: Parser LamExpr
lamExprParse = do
  exprs <- many1 (spaces *> (parenParse <|> applParse <|> abstrParse))
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
    traceM $ "abstrParse: " ++ [var]
    return (Abstr [var] body)

-- parses parentheses and returns expression inside them
parenParse :: Parser LamExpr
parenParse = do
    spaces
    char '('
    traceM "entered parenParse"
    expr <- lamExprParse <* spaces
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


mainLoop :: IO ()
mainLoop = do
    putStr ">>> "
    hFlush stdout
    input <- getLine
    if input == "q" then putStrLn "Exiting"
    else do 
        case parse parseTop "error" input of
            Right (Assign name expr) -> do
                putStrLn "Assignment"
                putStrLn (name ++ (show expr))
            Right (Var v) -> print (Var v)
            Right (Abstr v body) -> print (Abstr v body)
            Right (Appl l r) -> print (Appl l r)
            Left err -> putStrLn "Error while parsing"
        mainLoop
        

main :: IO ()
main = do
    putStrLn "Welcome to Lambda Calculus Interpreter"
    putStrLn "Enter 'q' to exit"
    mainLoop
    




-----  TESTS  -----
-------------------------------------------------------------------
test1 = parse parseTop "error" "\\f.\\x.fx"

test2 = parse parseTop "error" "\\f.\\x.f(fx)"

test3 = parse parseTop "error" "((\\x.x)((\\y.y)(\\y.y)))"

test4 = parse parseTop "error" "(\\x.x)((\\y.y)(\\y.y))"

test5 = parse parseTop "error" "             (\\x.x) ((\\y.y  ) (  \\y.y)  )"

test6 = parse parseTop "error" "  (\\x.x ) (  (\\y.y)   (\\y.y  )  )  "
