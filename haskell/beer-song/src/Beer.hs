module Beer (song) where

import Data.Char
import Data.List

bottles :: Int -> String
bottles 0 = "no more bottles of beer"
bottles 1 = "1 bottle of beer"
bottles n = show n ++ " bottles of beer"

action :: Int -> String
action 0 = "Go to the store and buy some more"
action 1 = "Take it down and pass it around"
action n = "Take one down and pass it around"

capitalize :: String -> String
capitalize s = toUpper (head s) : tail s

line :: Int -> Int -> String
line n r = capitalize $ bottles n ++ " on the wall, " ++ bottles n ++ ".\n" ++ action n ++ ", " ++ bottles r ++ " on the wall.\n"

song :: String
song = intercalate "\n" $ zipWith line [99, 98..0] ([98, 97..0] ++ [99])
