module MyTypes (
      VarId
    , ExprName
    , Env
    , SubstType(..)
    , Statement(..)
    , LamExpr(..)
    ) where

import Data.Map

type VarId = String     -- variable names inside lambda expression
type ExprName = String  -- lambda expression names
type Env = Map ExprName LamExpr   -- environment

data SubstType = LocalVar | ExprVar
    deriving (Show, Eq)

-- user input statements
data Statement = Expr LamExpr
               | Assign ExprName LamExpr
               deriving (Show, Eq)

-- lambda expressions
data LamExpr = Var VarId
             | ExprSubst VarId
             | Abstr VarId LamExpr
             | Appl LamExpr LamExpr
             deriving(Eq)

instance Show LamExpr where
    show (Var varId) = varId
    show (ExprSubst varId) = "$" ++ varId 
    show (Abstr varId expr) = "\\" ++ varId ++ "." ++ show expr
    show (Appl expr1 expr2) = 
        applyParentheses (show expr1) ++ " " ++ applyParentheses (show expr2)
            where
                applyParentheses str = if needsParentheses expr1 expr2 
                    then "(" ++ str ++ ")" 
                    else str
                needsParentheses (Appl _ _) _ = True
                needsParentheses _ (Appl _ _) = True
                needsParentheses _ _ = False