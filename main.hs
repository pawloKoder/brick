-- automatically generated by BNF Converter
module Main where

import System.IO ( stdin, stderr, hGetContents, hPutStrLn )
import System.Environment ( getArgs, getProgName )
import System.Exit

import Lexbrick
import Parbrick
import Skelbrick
import Printbrick
import Absbrick

import RunProgram

import ErrM


showTree :: (Show a, Print a) => a -> IO ()
showTree tree
 = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "No input file" >> exitWith (ExitFailure 66)
        [file] -> do
            source <- readFile file
            case pProgram . myLexer $ source of
                Bad s -> hPutStrLn stderr ("Parse failed: " ++ s) >> exitWith (ExitFailure 67)
                Ok tree -> do
                    let dump = return () --showTree tree
                    result <- runProgram tree
                    case result of
                        Left s -> hPutStrLn stderr ("Program failed with: " ++ s) >> dump >> exitWith (ExitFailure 68)
                        Right 0 -> exitSuccess
                        Right n -> exitWith $ ExitFailure $ fromIntegral n
        _ -> hPutStrLn stderr "Too many args" >> exitWith (ExitFailure 69)
