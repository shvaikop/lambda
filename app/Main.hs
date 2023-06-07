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



processSTDIN :: Map ExprName LamExpr -> IO ()
processSTDIN map = do 
    putStr ">>> "
    hFlush stdout
    input <- getLine
    case parseEntry input of
        Right (Assign name expr) -> do
            let map' = insert name expr map
            putStrLn "Assignment"
            processSTDIN map'
        Right (Expr expr) -> do
            print $ betaReduceTop $ fromJust $ expandSubstsTop map expr
            processSTDIN map
        Left err -> do
            print err
            processSTDIN map

processFIO :: Map ExprName LamExpr -> [String] -> IO ()
processFIO map [] = return ()
processFIO map (ln : rest) = do
    case parseEntry ln of
        Right (Assign name expr) -> do
            let map' = insert name expr map
            putStrLn "Assignment"
            processFIO map' rest
        Right (Expr expr) -> do
            print $ betaReduceTop $ fromJust $ expandSubstsTop map expr
            processFIO map rest
        Left err -> do
            print err
            processFIO map rest


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


-- "  (\x.x ) (  (  \y.y)   (\y.y  )  )  "
-- mainLoop :: Map ExprName LamExpr -> IO ()
-- mainLoop map = do
--     putStr ">>> "
--     hFlush stdout
--     input <- getLine
--     if input == "q" then putStrLn "Exiting"
--     else do
--         case parseEntry input of
--             Right (Assign name expr) -> do
--                 let map' = insert name expr map
--                 putStrLn "Assignment"
--                 mainLoop map'
--             Right (Expr expr) -> print $ betaReduceTop $ fromJust $ expandSubstsTop map expr
--             Left err -> print err
--         mainLoop map

-- processLine :: Bool -> String -> Map ExprName LamExpr -> IO ()
