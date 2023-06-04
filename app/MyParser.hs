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
import Text.Parsec.String ( Parser )
import Debug.Trace
import System.IO

import Data.Set

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
    -- traceM "entered statementAssignParse"
    name <- many1 letter
    symChar '='
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
    var <- symLetter
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
    symChar '('
    -- traceM "entered parenParse"
    expr <- lamExprParse
    symChar ')'
    -- traceM ("parenParse " ++ show expr)
    return expr

applParse :: Parser LamExpr
applParse = do
    appls <- many1 (varParse <|> parenParse)
    -- traceM ("applParse: " ++ show appls)
    return (foldl1 Appl appls)
    

--------------------------------------------------------------------

-- returns a set of all the free variables in a LamExpr
freeVars :: LamExpr -> Set VarId
freeVars (Var var) = singleton var
freeVars (Appl left right) = freeVars left `union` freeVars right
freeVars (Abstr var expr) = freeVars expr \\ singleton var

-- returns a set of all the variables in a LamExpr
allVars :: LamExpr -> Set VarId
allVars (Var var) = singleton var
allVars (Appl left right) = allVars left `union` allVars right
allVars (Abstr var expr) = singleton var `union` allVars expr

-- Generates an infinite list of strings with the following pattern
-- a,...,b,ab,...,az,ba,.....
varList :: [VarId]
varList = [c : suf | len <- [0..], c <- ['a'..'z'], suf <- varListHelp len]
    where varListHelp len 
            | len == 0 = [""]
            | otherwise = [c : suf| c <- ['a'..'z'], suf <- varListHelp (len - 1)]


-- substitution: subst x e expr means, in expr substitute e instead of variable x
subst :: VarId -> LamExpr -> LamExpr -> LamExpr 
subst x e (Var var)
    | x == var = e
    | otherwise = Var var

subst x e (Appl left right) = Appl (subst x e left) (subst x e right)

subst x e (Abstr var expr)
    | var == x = Abstr var expr -- abstraction binds the variable to be replaced
    | otherwise = Abstr var (subst x e expr)    -- NOT HANDLING CAPTURES
        where freeIn_e = freeVars e
              allIn_e = allVars e





---  TESTS  -----
-----------------------------------------------------------------

-- testing freeVars
expr1 = parse lamExprParse "error" "\\a.a b"
expr2 = parse lamExprParse "error" "\\c.\\d. w h"
expr3 = parse lamExprParse "error" "(\\f.f)((\\g.g)(\\k.g))"
free1 (Right expr) = freeVars expr
all1 (Right expr) = allVars expr

substTest var (Right e) (Right eMain) = subst var e eMain



test1 = parse parseTop "error" "\\f.\\x.fx"

test2 = parse parseTop "error" "\\f.\\x.f(fx)"

test3 = parse parseTop "error" "((\\x.x)((\\y.y)(\\y.y)))"

test4 = parse parseTop "error" "(\\x.x)((\\y.y)(\\y.y))"

test5 = parse parseTop "error" "             (\\x.x) ((\\y.y  ) (  \\y.y)  )"

test6 = parse parseTop "error" "  (\\x.x ) (  (\\y.y)   (\\y.y  )  )  "
test7 = parse parseTop "error" "x"
test8 = parse parseTop "error" "  x  "