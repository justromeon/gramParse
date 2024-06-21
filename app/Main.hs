{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encodeFile)
import System.IO (hFlush, stdout)
import Parser (play)


-- Main function
main :: IO ()
main = do
    -- Prompt user for input file name
    putStr "Enter the name of the input text file (inside 'input' directory): "
    hFlush stdout
    inputFileName <- getLine
    
    -- Read input file content
    input <- readFile $ "input/" ++ inputFileName
    
    -- Parse input using the Parser module
    case play input of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right parsed -> do
            -- Prompt user for output file name
            putStr "Enter the name for the output JSON file (inside 'output' directory): "
            hFlush stdout
            outputFileName <- getLine
            
            -- Write parsed JSON to output file
            encodeFile ("output/" ++ outputFileName) parsed
            
            putStrLn $ "Successfully parsed and wrote JSON to: output/" ++ outputFileName
