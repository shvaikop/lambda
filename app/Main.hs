-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}
module Main where

import System.IO

import MyParser

mainLoop :: IO ()
mainLoop = do
    putStr ">>> "
    hFlush stdout
    input <- getLine
    if input == "q" then putStrLn "Exiting"
    else do 
        case parseEntry input of
            Right (Assign name expr) -> do
                putStrLn "Assignment"
                putStrLn (name ++ " " ++ show expr)
            Right (Expr expr) -> print expr
            Left err -> putStrLn "Error while parsing"
        mainLoop
        

main :: IO ()
main = do
    putStrLn "Welcome to Lambda Calculus Interpreter"
    putStrLn "Enter 'q' to exit"
    mainLoop
    

-- "  (\x.x ) (  (\y.y)   (\y.y  )  )  "
