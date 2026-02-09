module Main where

import Backwords.Types
import Data.List
import Data.Char
import Data.Map (Map, fromList, toList, member, (!?), (!), insert)
import qualified Data.Map
import Data.Ratio
import Backwords.WordList
import Backwords.BasicGame

import Debug.Trace
import Test.Tasty.Bench

--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
--
-- USER ID: 5742529
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------

-- TODO: funcoiton oi daddded
filtermap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filtermap _ _ []        = []
filtermap f m xs  = [m x | x <- xs, f x]

headIfExists :: [a] -> Maybe a
headIfExists []   = Nothing
headIfExists list = Just $ head list

coalesceMaybe :: Maybe a -> a -> a
coalesceMaybe Nothing    def = def
coalesceMaybe (Just val) _   = val

applyPair :: ((a -> x),(b -> y)) -> (a,b) -> (x,y)
applyPair (f,g) (a,b) = (f a, g b)

freqmap :: Ord k => [k] -> Map k Integer
freqmap ls = foldl (\m l -> Data.Map.insert l ((coalesceMaybe (m !? l) 0) + 1) m) (fromList [] :: (Ord k => Map k Integer)) ls

freqsubset :: Ord k => Map k Integer -> Map k Integer -> Bool
freqsubset a b = and $ map (\(key,freq) -> (coalesceMaybe (a !? key) 0) >= freq) $ toList b


freqsubtract :: Ord k => k -> Integer -> Map k Integer -> Map k Integer
freqsubtract key n m
    = case m !? key of
        Nothing -> m
        Just c  -> if (c <= n) then Data.Map.delete key m else Data.Map.insert key (c - n) m


dup :: a -> (a,a)
dup x = (x,x)

-- Ex. 3:
-- Determine if a word is valid.
isValidWord :: String -> Bool
isValidWord w = elem (map toLower w) allWords

-- Ex. 4:
-- Determine the points value of a letter.
letterValue :: Char -> Int
letterValue l
    = case toLower l of
        'a' -> 1
        'e' -> 1
        'i' -> 1
        'l' -> 1
        'n' -> 1
        'o' -> 1
        'r' -> 1
        's' -> 1
        't' -> 1
        'u' -> 1

        'd' -> 2
        'g' -> 2

        'b' -> 3
        'c' -> 3
        'm' -> 3
        'p' -> 3

        'f' -> 4
        'h' -> 4
        'v' -> 4
        'w' -> 4
        'y' -> 4

        'k' -> 5

        'j' -> 8
        'x' -> 8

        'q' -> 10
        'z' -> 10

        _   -> 0

-- Ex. 5:
-- Score a word according to the Backwords scoring system.
scoreWord :: String -> Int
scoreWord []       = 0
scoreWord (l : ls) = letterValue l + 2 * scoreWord ls

-- Ex. 6:
-- Get all words that can be formed from the given letters.
allWordsLetterCounts :: [(Map Char Integer, String)]
allWordsLetterCounts = map (\w -> (freqmap w, w)) allWords

possibleWords :: [Char] -> [String]
possibleWords ls = map snd $ filter (freqsubset (freqmap ls) . fst) $ allWordsLetterCounts

allWordsLetterCounts2 :: [(Map Char Integer, (String, Int))]
allWordsLetterCounts2 = map (\w -> (freqmap w, (w, length w))) allWords

possibleWords2 :: [Char] -> [String]
possibleWords2 ls = map (fst . snd) $ filter (\(m,(_,l)) -> (l <= length ls) && freqsubset (freqmap ls) m) $ allWordsLetterCounts2

allWordsWithLengths :: [(String, Int)]
allWordsWithLengths = map (\w -> (w, length w)) allWords

maxWordLength :: Int
maxWordLength = foldl (\m (_,l) -> max m l) 0 allWordsWithLengths

allWordsLetterCounts3 :: [[(Map Char Integer, String)]]
allWordsLetterCounts3 = [ map (\(w,_) -> (freqmap w, w)) $ filter ((==) i . snd) allWordsWithLengths | i <- [0..maxWordLength] ]

-- TODO: complicated, but much faster
possibleWords3 ls = concat [ possibleWords3In ls $ allWordsLetterCounts3 !! i | i <- [3..(min maxWordLength $ length ls)] ]

possibleWords3In :: [Char] -> [(Map Char Integer, String)] -> [String]
possibleWords3In ls possible = map snd $ filter (freqsubset (freqmap ls) . fst) possible


-- Ex. 7:
-- Given a set of letters, find the highest scoring word that can be formed from them.
-- TODO: partial functions?
bestWord :: [Char] -> Maybe String
bestWord ls
    = case possibleWords ls of
        [] -> Nothing
        ws -> Just $ snd
                $ foldl max (0, "")
                $ map (\w -> (scoreWord w, w)) ws

-- Ex. 8:
-- Given a list of letters, and a word, mark as used all letters in the list that appear in the word.
useTiles :: [Char] -> String -> [Tile]
useTiles []       _    = []
useTiles (l : ls) w = (if elem l w then Used else Unused) l : (useTiles ls $ delete l w)

-- Ex. 9:
-- Given a nonempty bag of possible letters as a list, return the chance of drawing
-- each letter.
bagDistribution :: [Char] -> [(Char, Rational)]
bagDistribution ls
    = map (applyPair (id,(% (toInteger $ length ls))))
        $ toList
        $ freqmap ls
    where len = toInteger (length ls)

-- Ex. 9:
-- Write an AI which plays the Backwords game as well as possible.
aiMove :: [Char] -> [Char] -> Move
aiMove bag rack
    | length rack < 9
        = case countVowels bag of
            0         -> TakeConsonant
            otherwise -> if ((countVowels bag) == (length bag)) then TakeVowel else (if (countVowels rack) == 0 then TakeVowel else TakeConsonant)
    | otherwise
        = case bestWord rack of
            Nothing -> error "AAAAAAAA"
            Just w  -> PlayWord w
   where
        countVowels :: [Char] -> Int
        countVowels ls = length $ filter (`elem` "aeiou") ls


bestSequence :: [Char] -> (Int, [String])
bestSequence bag = bestSequence' (take 9 bag) (drop 9 bag)
    where
        bestSequence' :: [Char] -> [Char] -> (Int, [String])
        bestSequence' rack bag = foldl max (0, []) [ applyPair ((+) $ scoreWord w, (:) w) $ bestSequence' ((rack \\ w) ++ (take (length w) bag)) (drop (length w) bag) | w <- possibleWords3 rack ]


alp :: [Char]
alp = "abcdefghijklmnopqrstuvxyz"

isVowel :: Char -> Bool
isVowel l = elem l "aeiou"

vs :: [Char]
vs = filter isVowel alp

cs :: [Char]
cs = filter (not . isVowel) alp

vdist :: Map Char Rational
vdist = fromList $ bagDistribution vs

cdist :: Map Char Rational
cdist = fromList $ bagDistribution cs

bestWordFrom :: [String] -> (Int,String)
bestWordFrom ws = foldr max (0,"") $ map ( \w -> (scoreWord w,w) ) ws

avgScore :: [Char] -> [Char] -> Map Char Rational -> Rational
avgScore rack ls ldist
    = foldr ( \(l,(s,_)) acc -> (toRational s) * (ldist ! l) + acc ) 0
        $ map ( \(l,ws) -> (l,bestWordFrom ws) )
        $ foldr ( \l -> (:) (l,(possibleWords $ (l : rack))) ) [] ls

possibleWordsIncluding :: Char -> [Char] -> [String]
possibleWordsIncluding il [] = []
possibleWordsIncluding il ls = possibleWords3In ls $ map ( \(m,w) -> (freqsubtract il 1 m,w) ) $ filter ((member il) . fst) $ concat $ take (length ls + 1) allWordsLetterCounts3

sd :: [[(Integer,(Map Char Integer, String))]]
sd = map ( sortBy ( \a b -> compare b a ) . map ( \(m,w) -> (toInteger $ scoreWord w,(m,w)) ) ) allWordsLetterCounts3


avgScore2 :: [Char] -> [Char] -> Map Char Rational -> Rational
avgScore2 rack ls ldist
    = foldr ( \l acc ->
        acc
        + (ldist ! l)
        * (toRational
            $ fst
--            $ trace2 l
            $ foldr
                ( \list acc ->
                        max acc -- (trace2 l acc)
                            $ coalesceMaybe
                                (find
                                    ( \(s,(m,w)) -> fst acc > s || (freqsubset (freqmap (l : rack)) m && member l m) )
                                    list
                                )
                                (0,(fromList [],""))
                )
                (0,(fromList [],""))
                $ take (length rack + 1) sd
           )
        ) 0 ls

trace2 l x = trace (show l ++ "\t\t" ++ show x) x

main :: IO()
main = defaultMain
    [ bgroup "test"
        [ bench "1.1" $ nf (avgScore "aestr" cs) cdist
        , bench "1.2" $ nf (avgScore "aestr" vs) vdist
        , bench "2.1" $ nf (avgScore2 "aestr" cs) cdist
        , bench "1.2" $ nf (avgScore2 "aestr" vs) vdist
        ]
    ]

