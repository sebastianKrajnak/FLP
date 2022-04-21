{-
    Simplify context-free grammar algorithm application based on TIN algorithm 4.3
    Author: Sebastian Krajnak
    Date: 10.4.2022
-}

-- Inspired by TuringParse.hs demo from FLP class located at
-- https://wis.fit.vutbr.cz/FIT/st/cfs.php.cs?file=%2Fcourse%2FFLP-IT%2Fpclabs%2FTuring-machine%2FTuringParse.hs&cid=14578
module ParseInput where

import Control.Applicative ((<|>)) -- uncomment on local with ghc 8.10.7
-- import Control.Applicative ((<$>), (<*>), (<*), (<|>)) -- uncomment when using on Merlin
import Text.Parsec (char, endBy, eof, many1, newline, oneOf, sepBy1, string, count, parse)
import Text.Parsec.String (Parser)
import Control.Monad ((<=<))
import Control.Arrow (left)
import Data.Char (isUpper, isLower)

import Types

parseGrammar :: String -> Err Grammar 
parseGrammar = validate <=< left show . parse grammarParser ""

-- parses the whole BKG 
grammarParser :: Parser Grammar
grammarParser = Grammar 
        <$> parseNonTerminals <* newline
        <*> parseTerminals <* newline
        <*> parseStart <* newline
        <*> parseRules <* eof

-- parses a list of non-terminals
parseNonTerminals :: Parser [NonTerminal]
parseNonTerminals = sepBy1 parseNonTerminal comma

parseNonTerminal :: Parser NonTerminal 
parseNonTerminal = oneOf ['A'..'Z']

-- parses a list of terminals
parseTerminals :: Parser [Terminal]
parseTerminals = sepBy1 parseTerminal comma

parseTerminal :: Parser Terminal 
parseTerminal = oneOf ['a'..'z']

-- parses the starting non-terminal
parseStart :: Parser NonTerminal
parseStart = oneOf ['A'..'Z']

-- parses all the rules
parseRules :: Parser [Rule]
parseRules = endBy parseRule newline

parseRule :: Parser Rule
parseRule = Rule <$> oneOf ['A'..'Z'] <* string "->" <*> rightSideS
        where rightSideS = count 1 (char '#') <|> many1 (oneOf mixNonTerm)

-- parses the separator
comma :: Parser Char
comma = char ','

-- parses a mix of terminals and non-terminals (left side of a rule)
mixNonTerm :: TermNonTerminal
mixNonTerm = ['A'..'Z'] ++ ['a'..'z']

-- checks validity of a grammar
validate :: Grammar -> Err Grammar
validate bkg@(Grammar nonTerminalsS terminalsS startS rulesS) = if allOk then Right bkg else Left "Invlaid grammar"
        where
                allOk = 
                        all isUpper nonTerminalsS
                        && all isLower terminalsS 
                        && elem startS nonTerminalsS 
                        && not (null rulesS)     