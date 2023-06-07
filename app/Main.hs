-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}
module Main where

import System.IO
import System.Directory
import MyParser
import MyInterpreter
import Data.Map ( empty, insert, Map )
import Data.Maybe
import System.Environment
import Control.Monad


processInput :: Map ExprName LamExpr -> String -> IO (Map ExprName LamExpr)
processInput map input =
    case parseEntry input of
        Right (Assign name expr) -> do
            putStrLn "Assignment"
            return (insert name expr map)
        Right (Expr expr) -> do
            print $ betaReduceTop $ fromJust $ expandSubstsTop map expr
            return map
        Left err -> do
            print err
            return map

processSTDIN :: Map ExprName LamExpr -> IO ()
processSTDIN map = do
    putStr ">>> "
    hFlush stdout
    input <- getLine
    map' <- processInput map input
    processSTDIN map'

processFIO :: Map ExprName LamExpr -> [String] -> IO ()
processFIO map rest = foldM_ processInput map rest


main :: IO ()
main = do
    putStrLn "Welcome to Lambda Calculus Interpreter"
    args <- getArgs
    case args of
        [filename] -> do
            isValid <- doesFileExist filename
            if isValid
                then do
                    fileContent <- readFile filename
                    let fileLines = lines fileContent
                    processFIO empty fileLines
                else putStrLn "error: <filename> does not exist"
        [] -> do
            processSTDIN empty
        _ -> do
            putStrLn "usage: cabal run [-- filename]"