module Types where

import Data.List

type Terminal = Char  -- terminals in grammar
type NonTerminal = Char   -- non-terminals in grammar
type TermNonTerminal = Char   -- combination of terminals and non-terminals in grammar

-- data type for grammar rules with the form A -> B
data Rule = Rule {  
    leftSide :: NonTerminal,    -- left side of a rule, nonterminal
    rightSide :: [TermNonTerminal]    -- right side of a rule, can be either non-terminal or terminal
} deriving(Eq)

-- data type for context free grammar
data Grammar = Grammar {
    nonTerminals :: [NonTerminal],  -- list of all non-terminals in a grammar
    terminals :: [Terminal],    -- list of all terminals in a grammar
    start :: NonTerminal,   -- starting non-terminal symbol
    rules :: [Rule]     -- list of all rules in a grammar
} deriving (Eq)

type Err = Either String

instance Show Grammar where
    show (Grammar nonTerminalsS terminalsS startS rulesS) =
        unlines $ [intercalate "," (map (: []) nonTerminalsS)] ++ [intercalate "," (map (: []) terminalsS)] ++ [[startS]] ++ [unlines (map show rulesS)]

instance Show Rule where
    show (Rule leftSideS rightSideS) = [leftSideS] ++ "->" ++ rightSideS