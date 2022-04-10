{-
    Simplify context-free grammar algorithm application based on TIN algorithm 4.3
    Author: Sebastian Krajnak
    Date: 10.4.2022
-}

-- Inspired by TuringMain.hs demo from FLP class located at 
-- https://wis.fit.vutbr.cz/FIT/st/cfs.php.cs?file=%2Fcourse%2FFLP-IT%2Fpclabs%2FTuring-machine%2FTuringMain.hs&cid=14578
module Main (main) where

import System.Environment (getArgs)
-- import System.Exit (die)
-- import System.IO()

import Types
import ParseInput ( parseGrammar )
import SimplifyAlg ( stepOne )

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

die :: String -> IO a
die s = hPutStrLn stderr s >> exitFailure

-- Main program
main :: IO ()
main = do
    (action, input) <- processArgs =<< getArgs
    either die action (parseGrammar input)

-- Processes command line arguments such (switch option and input file)
processArgs :: [String] -> IO (Grammar -> IO (), String)
processArgs [x,y] = do
    input <- readFile y
    case x of
     "-i" -> return (printBKG, input)
     "-1" -> return (printCFGSimplify1, input)
     _    -> die ("unknown option " ++ x)
processArgs _ = die "expecting two arguments: [-i|-s] FILE"

-- Called when arg is -i, prints out the input grammar from internal representation with the same formating as the input
printBKG :: Grammar -> IO ()
printBKG grammar = do
    putStrLn "Printing loaded grammar..."
    putStr (show grammar)

-- Called when arg is -1, applies step 1 of algorithm 4.3 from TIN then prints out resulting grammar
printCFGSimplify1 :: Grammar  -> IO ()
printCFGSimplify1 grammar = printBKG $ stepOne grammar