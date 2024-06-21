{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encodeFile)
import System.IO (hFlush, stdout)
import Parser (play)

main :: IO ()
main = do
    putStr "Enter the name of the input text file (inside 'input' directory): "
    hFlush stdout
    inputFileName <- getLine
    input <- readFile $ "input/" ++ inputFileName
    
    case play input of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right parsed -> do
            putStr "Enter the name for the output JSON file (inside 'output' directory): "
            hFlush stdout
            outputFileName <- getLine
            
            encodeFile ("output/" ++ outputFileName) parsed
            putStrLn $ "Successfully parsed and wrote JSON to: output/" ++ outputFileName
