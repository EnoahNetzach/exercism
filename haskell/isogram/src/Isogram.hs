module Isogram (isIsogram) where

import Data.Char (toLower)

doubles :: String -> String -> Bool
doubles "" ""  = False
doubles (x:xs) "" = doubles xs [x]
doubles (x:"") ys = x `elem` ys
doubles xs ys
  | head xs `elem` ys = True
  | otherwise         = doubles (tail xs) (head xs : ys)

isIsogram :: String -> Bool
isIsogram = not . (`doubles` "") . map toLower . filter (`notElem` " -")
