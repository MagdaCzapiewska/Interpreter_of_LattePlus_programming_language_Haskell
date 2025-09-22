module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Interpreter
import TypeChecker
import AbsLattePlus
import LexLattePlus
import ParLattePlus
import ErrM

interpretFile :: FilePath -> IO ()
interpretFile file = do
    contents <- readFile file
    case pProgram (myLexer contents) of
        Ok tree -> do
            --putStrLn "Drzewo struktury programu:"
            --print tree
            case checkProgram tree of
                Right _ -> do
                    --putStrLn "Analiza typów zakończona pomyślnie"
                    result <- interpret tree
                    case result of
                        --Right ((returnValue, _), _) -> putStrLn $ "Wynik interpretacji: " ++ show returnValue
                        Right ((_, _), _) -> return ()
                        Left err -> hPutStrLn stderr $ "Interpreting error: " ++ show err
                Left err -> hPutStrLn stderr $ "Typechecking error: " ++ show err
        Bad err -> do
            hPutStrLn stderr $ "Parsing error: " ++ err
            --putStrLn "Drzewo struktury programu:"
            --print err

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            --putStrLn $ "Interpretacja pliku: " ++ file
            interpretFile file
        _ -> hPutStrLn stderr $ "Usage: interpreter <file_to_be_interpreted>"
