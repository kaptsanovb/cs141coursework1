module CourseworkOne where

import Backwords.Types
import Data.List
import Data.Char
import Data.Map (Map, fromList, toList, member, (!?), (!), insert, intersectionWith, update)
import qualified Data.Map
import Data.Ratio
import Backwords.WordList

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
    display str = intercalate "\n" [bound, letterRow str, bound]
        where
            bound = intercalate " " $ replicate (length str) "+---+"

            letterRow :: [Char] -> String
            letterRow (ch : [])   = ("| " ++ [toUpper ch] ++ " |")
            letterRow (ch : chs)  = ("| " ++ [toUpper ch] ++ " | ") ++ letterRow chs

-- Ex. 3:
-- Determine if a word is valid.
isValidWord :: String -> Bool
isValidWord w = elem lowerW allWords
        where
            firstL = toLower $ head w
            lowerW = map toLower w

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
--allWordsLetterCounts :: [(Map Char Integer, (String, Int))]
--allWordsLetterCounts = map (\w -> (freqmap w,(w,length w))) allWords

-- allWordsLetterCounts :: [(Map Char Integer, String)]
-- allWordsLetterCounts = map (\w -> (freqmap w, w)) allWords

--possibleWords :: [Char] -> [String]
--possibleWords ls = map (fst . snd) $ filter (\(m,(w,l)) -> (l <= length ls) && freqsubset (freqmap ls) m) $ allWordsLetterCounts

allWordsWithLengths :: [(String, Int)]
allWordsWithLengths = map (\w -> (w, length w)) allWords

maxWordLength :: Int
maxWordLength = foldl (\m (_,l) -> max m l) 0 allWordsWithLengths

allWordsLetterCounts :: [[(Map Char Integer, String)]]
allWordsLetterCounts = [ map (\(w,_) -> (freqmap w, w)) $ filter ((==) i . snd) allWordsWithLengths | i <- [0..maxWordLength] ]

-- TODO: complicated, but much faster
possibleWords ls = concat [ possibleWords' ls $ allWordsLetterCounts !! i | i <- [3..(min maxWordLength $ length ls)] ]
     where
        possibleWords' :: [Char] -> [(Map Char Integer, String)] -> [String]
        possibleWords' ls possible = map snd $ filter (freqsubset (freqmap ls) . fst) possible

{-allWordsLetterCounts2 :: [(Map Char Integer, (String, Int))]
allWordsLetterCounts2 = map (\w -> (freqmap w, (w, length w))) allWords

possibleWords :: [Char] -> [String]
possibleWords ls = map (fst . snd) $ filter (\(m,(_,l)) -> (l <= length ls) && freqsubset (freqmap ls) m) $ allWordsLetterCounts2
-}

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

worstWord :: [Char] -> Maybe String
worstWord ls
    = case possibleWords ls of
        [] -> Nothing
        ws -> Just $ snd
                $ foldl min (maxBound :: Int, "")
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
bagDistribution bag
    = map ( \(l,freq) -> (l, (freq % len)) )
        $ toList
        $ freqmap bag
    where len = toInteger $ length bag

-- Ex. 9:
-- Write an AI which plays the Backwords game as well as possible.
alp :: [Char]
alp = "abcdefghijklmnopqrstuvxyz"

isVowel :: Char -> Bool
isVowel l = elem l "aeiou"


sd :: [[(Integer,(Map Char Integer, String))]]
sd = map ( sortBy ( \a b -> compare b a ) . map ( \(m,w) -> (toInteger $ scoreWord w,(m,w)) ) ) allWordsLetterCounts

-- TODO: Sort by rank first, maybe bench?
avgScore :: [Char] -> [Char] -> Map Char Rational -> Rational
avgScore rack ls ldist
    = foldr ( \l acc ->
        acc
        + (coalesceMaybe (ldist !? l) 0)
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


aiMove :: [Char] -> [Char] -> Move
aiMove bag rack
    | length rack < 9           = takeTile
    | otherwise
        = case bestWord rack of
            Nothing -> error "ASKDHKAJHSDKH"
            Just w  -> PlayWord w
    where
        takeTile :: Move
        takeTile
            | length bag == 0               = error "SAHGKDSHFKDSHL"
            | countVowels bag == 0          = TakeConsonant
            | countVowels bag == length bag = TakeVowel
            | avgV >= avgC                  = TakeVowel
            | otherwise                     = TakeConsonant

        countVowels :: [Char] -> Int
        countVowels ls = length $ filter (`elem` "aeiou") ls

        avgV = avgScore rack vs vdist
        avgC = avgScore rack cs cdist

        vs :: [Char]
        vs = filter isVowel alp

        cs :: [Char]
        cs = filter (not . isVowel) alp

        vdist :: Map Char Rational
        vdist = fromList $ bagDistribution $ filter isVowel bag

        cdist :: Map Char Rational
        cdist = fromList $ bagDistribution $ filter (not . isVowel) bag



dist = freqmap initialBag
allPossible = filter (freqsubset dist . freqmap) allWords

{-
aiMove :: [Char] -> [Char] -> Move
aiMove bag rack
    | length rack < 9           = takeTile
    | otherwise
        = case possibleWords of
            Nothing ->
                case worstWord rack of
                    Nothing -> error "ASKDHKAJHSDKH"
                    Just w -> PlayWord w
            Just w  -> PlayWord w
    where
        takeTile :: Move
        takeTile
            | length bag == 0               = error "SAHGKDSHFKDSHL"
            | countVowels bag == 0          = TakeConsonant
            | countVowels bag == length bag = TakeVowel
            | isSubsequenceOf "mghrtz" rack = TakeVowel
            | length rack >= 7              = TakeVowel
            | otherwise                     = TakeConsonant

        countVowels :: [Char] -> Int
        countVowels ls = length $ filter (`elem` "aeiou") ls
-}

instance Show Move where
    show TakeConsonant = "c"
    show TakeVowel     = "v"
    show (PlayWord w)  = w
