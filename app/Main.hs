-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}
module Main where

import System.IO

import MyParser
import MyInterpreter
import Data.Map
import Data.Maybe

mainLoop :: Map ExprName LamExpr -> IO ()
mainLoop map = do
    putStr ">>> "
    hFlush stdout
    input <- getLine
    if input == "q" then putStrLn "Exiting"
    else do
        case parseEntry input of
            Right (Assign name expr) -> do
                let map' = insert name expr map
                putStrLn "Assignment"
                mainLoop map'
            Right (Expr expr) -> print $ betaReduceTop $ fromJust $ expandSubstsTop map expr
            Left err -> print err
        mainLoop map


main :: IO ()
main = do
    putStrLn "Welcome to Lambda Calculus Interpreter"
    putStrLn "Enter 'q' to exit"
    mainLoop empty


-- "  (\x.x ) (  (  \y.y)   (\y.y  )  )  "
