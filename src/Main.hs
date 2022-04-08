module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO(readFile, getLine, putStr, putStrLn, hFlush, stdout)

import Types
import ParseInput

main :: IO ()
main = do
    (action, input) <- processArgs =<< getArgs
    either die action (parseGrammar input)

-- Zpracování příkazového řádku
processArgs :: [String] -> IO (Grammar -> IO (), String)
processArgs [x,y] = do
    input <- readFile y
    case x of
     "-i" -> return (printBKG, input)
     _    -> die ("unknown option " ++ x)
processArgs _ = die "expecting two arguments: [-i|-s] FILE"

printBKG :: Grammar -> IO ()
printBKG grammar = do
    putStrLn "Printing loaded grammar..."
    putStr (show grammar)