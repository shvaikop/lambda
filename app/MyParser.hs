module MyParser
    ( VarId
    , ExprName
    , Statement(..)
    , LamExpr(..)
    , parseEntry
    ) where

import Text.Parsec
import Text.Parsec.String
import Debug.Trace
import System.IO

type VarId = String     -- variable names inside lambda expression
type ExprName = String  -- lambda expression names

-- user input statements
data Statement = Expr LamExpr
               | Assign ExprName LamExpr
               deriving (Show, Eq)

-- lambda expressions
data LamExpr = Var VarId
             | Abstr VarId LamExpr
             | Appl LamExpr LamExpr
             deriving(Show, Eq)

-- parses a char, skipping spaces before and after
sym :: Char -> Parser Char
sym c = spaces *> char c <* spaces

-- pubic function that main will call to parse
parseEntry :: String -> Either ParseError Statement
parseEntry arg = parse parseTop "error" arg

parseTop :: Parser Statement
parseTop = try statementAssignParse <|> statementExprParse

-- <name> = <expresssion>
statementAssignParse :: Parser Statement
statementAssignParse = do
    -- traceM "entered statementAssignParse"
    name <- many1 letter
    sym '='
    expr <- lamExprParse
    return (Assign name expr)

-- <expression>
statementExprParse :: Parser Statement
statementExprParse = do 
    -- traceM "entered statementExprParse"
    expr <- lamExprParse
    return (Expr expr)

lamExprParse :: Parser LamExpr
lamExprParse = do
  exprs <- many1 (spaces *> (parenParse <|> applParse <|> abstrParse <|> varParse))
  return (foldl1 Appl exprs)

-- parses a variable made up of a single character
varParse :: Parser LamExpr
varParse = do
    var <- letter <* spaces
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
    sym '('
    -- traceM "entered parenParse"
    expr <- lamExprParse
    sym ')'
    -- traceM ("parenParse " ++ show expr)
    return expr

applParse :: Parser LamExpr
applParse = do
    appls <- many1 ( spaces *> (varParse <|> parenParse) <* spaces )
    -- traceM ("applParse: " ++ show appls)
    return (foldl1 Appl appls)


---  TESTS  -----
-----------------------------------------------------------------
test1 = parse parseTop "error" "\\f.\\x.fx"

test2 = parse parseTop "error" "\\f.\\x.f(fx)"

test3 = parse parseTop "error" "((\\x.x)((\\y.y)(\\y.y)))"

test4 = parse parseTop "error" "(\\x.x)((\\y.y)(\\y.y))"

test5 = parse parseTop "error" "             (\\x.x) ((\\y.y  ) (  \\y.y)  )"

test6 = parse parseTop "error" "  (\\x.x ) (  (\\y.y)   (\\y.y  )  )  "
test7 = parse parseTop "error" "x"
test8 = parse parseTop "error" "  x  "