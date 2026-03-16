module EarsUtils where

import Text.Pandoc.Builder (Blocks, fromList, toList)
import Data.Char (toUpper)
import Data.List (intercalate, intersperse)

joinWords :: Bool -> [String] -> String
joinWords _ [] = ""
joinWords isStart (w : ws) = intercalate " " ((capitalize isStart w) : ws)

capitalize :: Bool -> String -> String
capitalize _ [] = []
capitalize False x = x
capitalize True (x:xs) = (toUpper x) : xs

andify :: [String] -> [String]
andify = intersperse "and"

flatify :: [[String]] -> [String]
flatify [] = []
flatify [[]] = []
flatify [x] = [joinWords False x]
flatify ([]:xs) = flatify xs
flatify (x:xs) = ((joinWords False x) ++ ", ") : (flatify xs)

makeTokens :: [String] -> [String] -> [String]
makeTokens _ [] = []
makeTokens prefixes tokens = prefixes ++ (andify tokens)

prependToShall :: [String] -> [String] -> [String]
prependToShall _ [] = []
prependToShall [] tokens = tokens
prependToShall prefixes (s@("shall") : tokens) = prefixes ++ [s] ++ tokens
prependToShall prefixes (token:tokens) = token : (prependToShall prefixes tokens)

flatifyBlocks :: [Blocks] -> Blocks
flatifyBlocks = fromList . concat . (map toList)

