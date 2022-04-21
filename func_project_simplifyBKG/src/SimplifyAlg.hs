{-
    Simplify context-free grammar algorithm application based on TIN algorithm 4.3
    Author: Sebastian Krajnak
    Date: 10.4.2022
-}

{-# LANGUAGE RecordWildCards #-}
module SimplifyAlg where
import Types

-- Step 1 of algorithm 4.3 from TIN's main support (opora) document
-- Removal of non-terminating non-terminals and adjusting rules to only contain filtered non-terminals
stepOne :: Grammar -> Grammar
stepOne bkg@Grammar{..} =
    Grammar {nonTerminals=newNonTerminals, terminals=terminals, start=start, rules=newRules}
    where
        newNonTerminals = getNewNonterminals bkg []
        newRules = filterRules bkg newNonTerminals

-- (step 1 of algorithm 4.3 from TIN's lecture on context-free grammars from winter semester 2021
-- located at http://www.fit.vutbr.cz/study/courses/TIN/public/Prednasky/tin-pr03-bj1.pdf )
-- Algorithm 4.1 create a new set containing only terminating non-terminals
-- Step 3 of said algorithm, if the previous and next set of non-terminals are the same, then the wanted set was the previous one
-- if they are not equal, create a new set and recursiverly try again
getNewNonterminals :: Grammar -> [NonTerminal] -> [NonTerminal]
getNewNonterminals grammar prevN = if prevN == nextN then prevN else getNewNonterminals grammar (rmdups (prevN ++ nextN))
    where
        nextN = getNextN grammar (prevN ++ terminals grammar ++ ['#'])

-- Generate new set of non-terminals based on step 2 of algorithm 4.1
getNextN :: Grammar -> TermNonTerminal -> [NonTerminal]
getNextN grammar alpha = [nonterm | nonterm <- nonTerminals grammar, isRuleInRules grammar alpha nonterm]

-- Helper function for getNextN, checks whether rule is in the set of rules according to step 2 of alg. 4.1
isRuleInRules :: Grammar -> TermNonTerminal -> NonTerminal -> Bool
isRuleInRules grammar alpha nonterm = if null [rule' |
    rule' <- rules grammar,
    leftSide rule' == nonterm,
    leftSide rule' `elem` nonTerminals grammar,
    checkAlpha (rightSide rule') alpha]
    then False
    else True

-- Helper function for isRuleInRules, checks if the rule's right side is made up of an iteration of previous non-terminals
-- and terminals according to step 2 of alg. 4.1
-- VSCode wants to reduce the whole "and $ .." part to "all (`elem` alpha) rightSideR", but I wouldn't think of that
-- and it is a cool Haskell thing to remember so that's why this comment exists
checkAlpha :: TermNonTerminal -> TermNonTerminal -> Bool
checkAlpha rightSideR alpha = and $ map (\x -> if x `elem` alpha then True else False) rightSideR 

-- Remove duplicates function which preserves order, author: Graham Hutton on p.86 in Programming in Haskell 
-- needed this because nub wasn't working properly
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- (step 2 of algorithm 4.3 from TIN's lecture on context-free grammars from winter semester 2021, link above)
-- Filter rules so they only feature newly found terminating non-terminals on both sides
filterRules :: Grammar -> [NonTerminal] -> [Rule]
filterRules grammar nonTerms = [rule' | 
    rule' <- rules grammar, 
    leftSide rule' `elem` nonTerms, 
    checkAlpha (rightSide rule') (terminals grammar ++ nonTerms)]