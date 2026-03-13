module EarsUtils where

import Data.Char (toUpper)
import Data.List (intercalate, intersperse)

joinWords :: [String] -> String
joinWords = intercalate " "

capitalize :: Bool -> String -> String
capitalize _ [] = []
capitalize False x = x
capitalize True (x:xs) = (toUpper x) : xs

makeStatement :: Bool -> [String] -> String
makeStatement _ [] = []
makeStatement isStart (word : words) = intercalate " " ((capitalize isStart word) : words) ++ "."

andify :: [String] -> [String]
andify = intersperse "and"



