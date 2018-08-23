module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

toLowStr :: String -> String
toLowStr = map toLower

hash :: String -> String
hash = sort . toLowStr

anagramsFor :: String -> [String] -> [String]
anagramsFor subject = filter ((==) (hash subject) . hash) . filter ((/=) (toLowStr subject) . toLowStr)