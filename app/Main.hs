-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}
module Main where

import System.IO ( hFlush, stdout )
import System.Directory ( doesFileExist )
import MyParser
import MyInterpreter
import MyTypes
import Data.Map ( empty, insert, Map )
-- import Data.Maybe
import System.Environment ( getArgs )
import Control.Monad ( foldM_ )
import Data.Maybe


processInput :: Env -> String -> IO Env
processInput map input =
    case parseEntry input of
        Right (Assign name expr) -> do
            let exp_expr = expandSubstsTop map expr
            case exp_expr of
                Just expr' -> return (insert name expr' map)
                Nothing -> do 
                    print "Expression contains unknown variable"
                    return map

        Right (Expr expr) -> do
            let exp_expr = expandSubstsTop map expr
            case exp_expr of
                Just expr' -> print $ betaReduceTop map expr'
                Nothing -> print "Expression contains unknown variable"
            return map

        Left err -> do
            print err
            return map

processSTDIN :: Env -> IO ()
processSTDIN map = do
    putStr ">>> "
    hFlush stdout
    input <- getLine
    map' <- processInput map input
    processSTDIN map'

processFIO :: Env -> [String] -> IO ()
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