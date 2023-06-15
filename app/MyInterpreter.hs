module MyInterpreter (
      expandSubstsTop,
      betaReduceTop
    ) where

import Data.Set ( (\\), member, singleton, toList, union, Set )
import Data.Maybe ()
import Data.List ((\\))
import Data.Map (Map, empty, lookup, member, (!), insert)
import Debug.Trace

import MyTypes

-- returns a set of all the free variables in a LamExpr
freeVars :: LamExpr -> Set VarId
freeVars (Var var) = singleton var
freeVars (Appl left right) = freeVars left `union` freeVars right
freeVars (Abstr var expr) = freeVars expr Data.Set.\\ singleton var

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
subst :: VarId -> LamExpr -> LamExpr -> SubstType -> LamExpr
subst x e (Var var) sub_type
    | sub_type == LocalVar && x == var = e
    | otherwise = Var var

subst x e (ExprSubst var) sub_type
    | sub_type == ExprVar && x == var = e
    | otherwise = ExprSubst var

subst x e (Appl left right) sub_type = Appl (subst x e left sub_type) (subst x e right sub_type)

subst x e (Abstr var expr) sub_type
    | var == x = Abstr var expr -- abstraction binds the variable to be replaced
    | Data.Set.member var freeIn_e = Abstr freshVar (subst x e expr' sub_type)  -- replace var with a fresh variable name, subsitute it for y everywhere in the expression
    | not (Data.Set.member var freeIn_e) = Abstr var (subst x e expr sub_type)
        where freeIn_e = freeVars e -- set of free variables in e           -- DONE REDUNDANT NUMBER OF TIMES, COULD BE DONE ONLY ONCE
              allIn_e = allVars e   -- set of all variables in e
              allIn_expr = allVars expr     -- set of all variables in expr
              freshVar = head (varList Data.List.\\ toList (allIn_e `union` allIn_expr)) -- new variable name for var (\var. ...)
              expr' = subst var (Var freshVar) expr sub_type -- replace all instances of var with freshVar


-- expands all the ExprSubts in an expressions with their corresponding expressions
-- returns Nothing if expression with some name does not exist
expandSubstsTop :: Map ExprName LamExpr -> LamExpr -> Maybe LamExpr
expandSubstsTop map expr
    | all_expr_names == [] = Just expr
    | not doAllExist = Nothing
    | otherwise = expandSubstsTop map expanded
        where all_expr_names = allExprSubsts expr []
              doAllExist = allExprsExist map all_expr_names
              expanded = expandSubsts map all_expr_names expr


expandSubsts :: Map ExprName LamExpr -> [VarId] -> LamExpr -> LamExpr
expandSubsts map [] expr = expr
expandSubsts map (x:xs) expr = subst x e (expandSubsts map xs expr) ExprVar
    where e = map ! x

-- returns a list of names of all expressions that have to substituted
allExprSubsts :: LamExpr -> [VarId] -> [VarId]
allExprSubsts (Var x) ls = ls
allExprSubsts (Abstr x expr) ls = allExprSubsts expr ls
allExprSubsts (Appl left right) ls = let left_ls = allExprSubsts left ls
                                         right_ls = allExprSubsts right left_ls
                                     in right_ls
allExprSubsts (ExprSubst x) ls = x:ls

-- return True if all expressions with names in [VarId] exist in Map
allExprsExist :: Map ExprName LamExpr -> [VarId] -> Bool
allExprsExist map ls = and [Data.Map.member var map | var <- ls]


-- Applies successive beta reduction in normal order
betaReduceTop :: LamExpr -> LamExpr
betaReduceTop expr =
    let reducedExpr = betaReduce expr
    in if expr == reducedExpr
        then reducedExpr
        else betaReduceTop reducedExpr

-- Performs all possible left-most beta reductions
betaReduce :: LamExpr -> LamExpr
betaReduce arg@(Appl (Abstr x expr) e) = subst x e expr LocalVar
betaReduce (Appl left right) =
    let left_reduced = betaReduce left
        right_reduced = betaReduce right
    in if left == left_reduced
        then Appl left (betaReduce right)
        else Appl left_reduced right
    -- Appl (betaReduce left) (betaReduce right)
betaReduce (Abstr x expr) = 
    Abstr x (betaReduce expr)
betaReduce expr = expr


-- Performs all possible left-most beta reductions
-- betaReduce :: LamExpr -> LamExpr
-- betaReduce arg@(Appl (Abstr x expr) e) =
--     trace ("Reducing: " ++ show arg) $
--     subst x e expr LocalVar
-- betaReduce (Appl left right) =
--     let reducedLeft = betaReduce left
--         reducedRight = betaReduce right
--     in trace ("Reducing: " ++ show (Appl reducedLeft reducedRight)) $
--         Appl reducedLeft reducedRight
-- betaReduce (Abstr x expr) =
--     trace ("Reducing: " ++ show (Abstr x expr)) $
--     Abstr x (betaReduce expr)
-- betaReduce expr = expr
