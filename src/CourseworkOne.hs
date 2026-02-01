module CourseworkOne where

import Backwords.Types
import Data.List
import Data.Char
import Data.Map (Map, fromList, toList, member, (!?), insert)
import Data.Ratio
import Backwords.WordList

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
filtermap f m (x : xs)  = if f x then m x : (filtermap f m xs) else filtermap f m xs

headIfExists :: [a] -> Maybe a
headIfExists []   = Nothing
headIfExists list = Just $ head list

coalesceMaybe :: Maybe a -> a -> a
coalesceMaybe Nothing    def = def
coalesceMaybe (Just val) _   = val

applyPair :: ((a -> x),(b -> y)) -> (a,b) -> (x,y)
applyPair (f,g) (a,b) = (f a, g b)


-- Ex. 1:
-- Read the spec to find out what goes here.
-- TODO: maybe add types
instance Display Char where
    -- Backslash characters used to prevent a comment from forming TODO: exaplin better
    display ch = "+---+\n| " ++ [toUpper ch] ++ " |\n+---+"

-- Ex. 2:
-- Read the spec to find out what goes here.
instance Display [Char] where
    -- TODO: rename
    display []  = ""
    display str = intercalate "\n" [bound, intercalate " " $ letterRow str, bound]
        where
            bound = intercalate " " $ replicate (length str) "+---+"

            letterRow :: [Char] -> [[Char]]
            letterRow []         = []
            letterRow (ch:chs)  = ("| " ++ [toUpper ch] ++ " |") : letterRow chs

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
possibleWords :: [Char] -> [String]
possibleWords ls = possibleWords' ls $ filter ((<= length ls) . length) allWords
    where
        possibleWords' :: [Char] -> [String] -> [String]
        possibleWords' _  []       = []
        possibleWords' [] possible = filter ("" ==) possible
        possibleWords' ls possible = filter ("" ==) possible ++ concat [ map (l :) $ possibleWords' (newLs l) $ newPossible l | l <- nub ls ]
            where
                newLs l = delete l ls
                newPossible l = map tail $ filter ((==) (Just l) . headIfExists) possible

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
        $ foldl (\m l -> Data.Map.insert l ((coalesceMaybe (m !? l) 0) + 1) m) (fromList []) ls
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

instance Show Move where
    show TakeConsonant = "c"
    show TakeVowel     = "v"
    show (PlayWord w)  = w


alp = "qwertyuiopasdfghjklzxcvbnm"
main :: IO()
main = defaultMain
    [ bgroup "test"
        [ bench "bestWord"      $ nf bestWord alp
        , bench "possibleWords" $ nf possibleWords alp
        , bench "filter"        $ nf (filter ((<= (length alp)) . length)) allWords
        ]
    ]

