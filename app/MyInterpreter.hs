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
freeVars :: Env -> LamExpr -> Set VarId
freeVars map (Var var) = singleton var
freeVars map (Appl left right) = freeVars map left `union` freeVars map right
freeVars map (Abstr var expr) = freeVars map expr Data.Set.\\ singleton var
freeVars map (ExprSubst var) = let e = map ! var
                               in freeVars map e

-- returns a set of all the variables in a LamExpr
allVars :: Env -> LamExpr -> Set VarId
allVars map (Var var) = singleton var
allVars map (Appl left right) = allVars map left `union` allVars map right
allVars map (Abstr var expr) = singleton var `union` allVars map expr
allVars map (ExprSubst var) = let e = map ! var
                               in allVars map e

-- Generates an infinite list of strings with the following pattern
-- a,...,b,ab,...,az,ba,.....
varList :: [VarId]
varList = concatMap varListHelp [1..]
    where varListHelp len
            | len == 0 = [""]
            | otherwise = [c : suf| c <- ['a'..'z'], suf <- varListHelp (len - 1)]


-- substitution: subst x e expr means, in expr substitute e instead of variable x
subst :: Env -> VarId -> LamExpr -> LamExpr -> SubstType -> LamExpr
subst map x e (Var var) sub_type
    | sub_type == LocalVar && x == var = e
    | otherwise = Var var

subst map x e (ExprSubst var) sub_type
    | sub_type == ExprVar && x == var = e
    | otherwise = ExprSubst var

subst map x e (Appl left right) sub_type = 
    Appl (subst map x e left sub_type) (subst map x e right sub_type)

subst map x e (Abstr var expr) sub_type
    -- abstraction binds the variable to be replaced
    | var == x = Abstr var expr 
    -- replace var with a fresh variable name, subsitute it for y everywhere in the expression
    | Data.Set.member var freeIn_e = 
        Abstr freshVar (subst map x e expr' sub_type) 
    | not (Data.Set.member var freeIn_e) = 
        Abstr var (subst map x e expr sub_type)
            where   
                freeIn_e = freeVars map e -- set of free variables in e 
                allIn_e = allVars map e   -- set of all variables in e
                allIn_expr = allVars map expr     -- set of all variables in expr
                -- new variable name for var (\var. ...)
                freshVar = head (varList Data.List.\\ toList (allIn_e `union` allIn_expr)) 
                -- replace all instances of var with freshVar
                expr' = subst map var (Var freshVar) expr sub_type 


-- expands all the ExprSubts in an expressions with their corresponding expressions
-- returns Nothing if expression with some name does not exist
expandSubstsTop :: Env -> LamExpr -> Maybe LamExpr
expandSubstsTop map expr
    | all_expr_names == [] = Just expr
    | not doAllExist = Nothing
    | otherwise = expandSubstsTop map expanded
        where all_expr_names = allExprSubsts expr []
              doAllExist = allExprsExist map all_expr_names
              expanded = expandSubsts map all_expr_names expr


expandSubsts :: Env -> [VarId] -> LamExpr -> LamExpr
expandSubsts map [] expr = expr
expandSubsts map (x:xs) expr = subst map x e (expandSubsts map xs expr) ExprVar
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
allExprsExist :: Env -> [VarId] -> Bool
allExprsExist map ls = and [Data.Map.member var map | var <- ls]


-- Applies successive beta reduction in normal order
betaReduceTop :: Env -> LamExpr -> LamExpr
betaReduceTop map expr =
    let reducedExpr = betaReduce map expr
    in if expr == reducedExpr
        then reducedExpr
        else betaReduceTop map reducedExpr

-- Performs all possible left-most beta reductions
-- Only performs one leftmost reduction
betaReduce :: Env -> LamExpr -> LamExpr
betaReduce map arg@(Appl (Abstr x expr) e) = subst map x e expr LocalVar
betaReduce map (Appl left right) =
    let left_reduced = betaReduce map left
        right_reduced = betaReduce map right
    in if left == left_reduced
        then Appl left (betaReduce map right)
        else Appl left_reduced right
betaReduce map (Abstr x expr) = 
    Abstr x (betaReduce map expr)
betaReduce map expr = expr
