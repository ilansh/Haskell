

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _             = True
formableBy (x:xs) []        = False
formableBy (x:xs) hand    = (x `elem` hand) && (formableBy xs (delete x hand))

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (flip formableBy hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ []                    = True
wordFitsTemplate [] _ (z:zs)                = False
wordFitsTemplate (x:xs) _ []                = False
wordFitsTemplate template [] word           = template == word
wordFitsTemplate (x:xs) hand (z:zs)         = ((x == z) && (wordFitsTemplate xs hand zs)) || ((x == '?') && (z `elem` hand) && (wordFitsTemplate xs (delete z hand) zs))

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (wordFitsTemplate template hand) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord []        = 0
scrabbleValueWord (x:xs)    = (scrabbleValue x) + (scrabbleValueWord xs)

bestWords :: [String] -> [String]
bestWords []     = []
bestWords words  = bestWordsHelper 0 [] words

--Second string param is an accumulator, first Int is the max so far
bestWordsHelper :: Int -> [String] -> [String] -> [String]
bestWordsHelper _ [] []         = []
bestWordsHelper _ bestSoFar []  = bestSoFar
bestWordsHelper m bestSoFar (x:xs)
    | val > m       = bestWordsHelper val [x] xs 
    | val == m      = bestWordsHelper val (x:bestSoFar) xs
    | otherwise     = bestWordsHelper m bestSoFar xs
    where
        val =  (scrabbleValueWord x)
        
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate stemplate word = scrabbleValueTemplateHelper 0 1 stemplate word
   
    
scrabbleValueTemplateHelper :: Int -> Int -> STemplate -> String -> Int
scrabbleValueTemplateHelper curSum wordMul [] [] = wordMul * curSum
scrabbleValueTemplateHelper curSum wordMul (x:xs) (y:ys)
    | x == 'D'  = scrabbleValueTemplateHelper (yVal * 2 + curSum) wordMul xs ys  
    | x == 'T'  = scrabbleValueTemplateHelper (yVal * 3 + curSum) wordMul xs ys
    | x == '2'  = scrabbleValueTemplateHelper (curSum + yVal) (wordMul * 2) xs ys
    | x == '3'  = scrabbleValueTemplateHelper (curSum + yVal) (wordMul * 3) xs ys
    | otherwise = scrabbleValueTemplateHelper (yVal + curSum) wordMul xs ys 
    where
        yVal    = scrabbleValue y