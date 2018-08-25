module Scrabble (scoreLetter, scoreWord) where

import Data.Char

scoreLetter :: Char -> Integer
scoreLetter l
  | c `elem` "AEIOULNRST" = 1
  | c `elem` "DG"         = 2
  | c `elem` "BCMP"       = 3
  | c `elem` "FHVWY"      = 4
  | c `elem` "K"          = 5
  | c `elem` "JX"         = 8
  | c `elem` "QZ"         = 10
  | otherwise             = 0
    where c = toUpper l

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
