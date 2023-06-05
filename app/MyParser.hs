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
-- import Debug.Trace
-- import System.IO
import Data.Set ()
import Data.Maybe ()
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
  exprs <- many1 (spaces *> (parenParse <|> applParse <|> abstrParse <|> varParse) <* spaces)
  return (foldl1 Appl exprs)

-- parses a local (single char) variable or an expression name ($...)
varParse :: Parser LamExpr
varParse = localVarParse <|> exprSubstParse

-- parses a variable made up of a single character
localVarParse :: Parser LamExpr
localVarParse = do
    var <- symLetter
    -- traceM $ "varParse: " ++ [var]
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

-- -- returns a set of all the free variables in a LamExpr
-- freeVars :: LamExpr -> Set VarId
-- freeVars (Var var) = singleton var
-- freeVars (Appl left right) = freeVars left `union` freeVars right
-- freeVars (Abstr var expr) = freeVars expr Data.Set.\\ singleton var

-- -- returns a set of all the variables in a LamExpr
-- allVars :: LamExpr -> Set VarId
-- allVars (Var var) = singleton var
-- allVars (Appl left right) = allVars left `union` allVars right
-- allVars (Abstr var expr) = singleton var `union` allVars expr

-- -- Generates an infinite list of strings with the following pattern
-- -- a,...,b,ab,...,az,ba,.....
-- varList :: [VarId]
-- varList = [c : suf | len <- [0..], c <- ['a'..'z'], suf <- varListHelp len]
--     where varListHelp len
--             | len == 0 = [""]
--             | otherwise = [c : suf| c <- ['a'..'z'], suf <- varListHelp (len - 1)]


-- -- substitution: subst x e expr means, in expr substitute e instead of variable x
-- subst :: VarId -> LamExpr -> LamExpr -> SubstType -> LamExpr
-- subst x e (Var var) sub_type
--     | sub_type == LocalVar && x == var = e
--     | otherwise = Var var

-- subst x e (ExprSubst var) sub_type
--     | sub_type == ExprVar && x == var = e
--     | otherwise = ExprSubst var

-- subst x e (Appl left right) sub_type = Appl (subst x e left sub_type) (subst x e right sub_type)

-- subst x e (Abstr var expr) sub_type
--     | var == x = Abstr var expr -- abstraction binds the variable to be replaced
--     | Data.Set.member var freeIn_e = Abstr freshVar (subst x e expr' sub_type)  -- replace var with a fresh variable name, subsitute it for y everywhere in the expression
--     | not (Data.Set.member var freeIn_e) = Abstr var (subst x e expr sub_type)
--         where freeIn_e = freeVars e -- set of free variables in e           -- DONE REDUNDANT NUMBER OF TIMES, COULD BE DONE ONLY ONCE
--               allIn_e = allVars e   -- set of all variables in e
--               allIn_expr = allVars expr     -- set of all variables in expr
--               freshVar = head (varList Data.List.\\ toList (allIn_e `union` allIn_expr)) -- new variable name for var (\var. ...)
--               expr' = subst var (Var freshVar) expr sub_type -- replace all instances of var with freshVar


-- -- expands all the ExprSubts in an expressions with their corresponding expressions
-- -- returns Nothing if expression with some name does not exist
-- expandSubstsTop :: Map ExprName LamExpr -> LamExpr -> Maybe LamExpr
-- expandSubstsTop map expr
--     | all_expr_names == [] = Just expr
--     | not doAllExist = Nothing
--     | otherwise = expandSubstsTop map expanded
--         where all_expr_names = allExprSubsts expr []
--               doAllExist = allExprsExist map all_expr_names
--               expanded = expandSubsts map all_expr_names expr


-- expandSubsts :: Map ExprName LamExpr -> [VarId] -> LamExpr -> LamExpr
-- expandSubsts map [] expr = expr
-- expandSubsts map (x:xs) expr = subst x e (expandSubsts map xs expr) ExprVar
--     where e = map ! x

-- -- returns a list of names of all expressions that have to substituted
-- allExprSubsts :: LamExpr -> [VarId] -> [VarId]
-- allExprSubsts (Var x) ls = ls
-- allExprSubsts (Abstr x expr) ls = allExprSubsts expr ls
-- allExprSubsts (Appl left right) ls = let left_ls = allExprSubsts left ls
--                                          right_ls = allExprSubsts right left_ls
--                                      in right_ls
-- allExprSubsts (ExprSubst x) ls = x:ls

-- -- return True if all expressions with names in [VarId] exist in Map
-- allExprsExist :: Map ExprName LamExpr -> [VarId] -> Bool
-- allExprsExist map ls = and [Data.Map.member var map | var <- ls]


-- -- Applies successive beta reduction in normal order
-- betaReduceTop :: LamExpr -> LamExpr
-- betaReduceTop expr =
--     let reducedExpr = betaReduce expr
--     in if expr == reducedExpr
--         then expr
--         else betaReduce reducedExpr

-- -- Performs all possible left-most beta reductions
-- -- Not sure if this completely adheres to normal order reduction
-- betaReduce :: LamExpr -> LamExpr
-- betaReduce (Appl (Abstr x expr) e) = subst x e expr LocalVar
-- betaReduce (Appl left right) =
--     Appl (betaReduce left) (betaReduce right)
-- betaReduce (Abstr x expr) = Abstr x (betaReduce expr)
-- betaReduce expr = expr

-- betaReduce :: LamExpr -> LamExpr
-- betaReduce (Appl (Abstr x expr) e) =
--     let substitutedExpr = subst x e expr
--     in trace ("Reducing application with abstraction: " ++ show (Appl (Abstr x expr) e))
--        $ trace ("Substituted expression: " ++ show substitutedExpr)
--        $ substitutedExpr
-- betaReduce (Appl left right) =
--     let reducedLeft = betaReduce left
--         reducedRight = betaReduce right
--     in trace ("Reducing application: " ++ show (Appl reducedLeft reducedRight))
--        $ Appl reducedLeft reducedRight
-- betaReduce (Abstr x expr) =
--     let reducedExpr = betaReduce expr
--     in trace ("Reducing abstraction: " ++ show (Abstr x reducedExpr))
--        $ Abstr x reducedExpr
-- betaReduce expr = expr





---  TESTS  -----
-----------------------------------------------------------------

-- testing freeVars
-- expr1 = parse lamExprParse "error" "\\a.a k"
-- expr2 = parse lamExprParse "error" "\\c.\\d. w h"
-- expr3 = parse lamExprParse "error" "(\\f.f)((\\g.g)(\\k.g k))"
-- free1 (Right expr) = freeVars expr
-- all1 (Right expr) = allVars expr

-- expr4 = parse lamExprParse "error" "((\\x.x)((\\y.y)(\\y.y)))"

-- expr5 = parse lamExprParse "error" "(\\x.\\y.y x)(\\z.u)"
-- expr5' = parse lamExprParse "error" "(\\x.\\y.y x)(\\x.y)"

-- expr6 = parse lamExprParse "error" "(\\x.x x) (\\z.u)"
-- expr7 = parse lamExprParse "error" "\\x.(\\x.x y) x"

-- expr8 = parse lamExprParse "error" "\\x.(\\y.x y) (\\x.y)"

-- expr9 = parse lamExprParse "error" "(\\x.(\\y.x y) (\\x.(\\y.x y) z))" -- Works!!!

-- expr10 = parse lamExprParse "error" "\\y.(\\z.\\y.z) (\\x.y)"   -- testing variable renaming, works
-- substTest :: VarId -> Either a1 LamExpr -> Either a2 LamExpr -> LamExpr
-- substTest var (Right e) (Right eMain) = subst var e eMain LocalVar   -- testing subst with substituting expr1 for "g" in expr3
-- betaTest (Right e) = betaReduceTop e





-- test1 = parse parseTop "error" "\\f.\\x.fx"

-- test2 = parse parseTop "error" "\\f.\\x.f(fx)"

-- test3 = parse parseTop "error" "((\\x.x)((\\y.y)(\\y.y)))"

-- test4 = parse parseTop "error" "(\\x.x)((\\y.y)(\\y.y))"

-- test5 = parse parseTop "error" "             (\\x.x) ((\\y.y  ) (  \\y.y)  )"

-- test6 = parse parseTop "error" "  (\\x.x ) (  (\\y.y)   (\\y.y  )  )  "
-- test7 = parse parseTop "error" "x"
-- test8 = parse parseTop "error" "  x  "