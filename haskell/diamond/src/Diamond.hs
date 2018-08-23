module Diamond (diamond) where

import Data.Char (chr, isLetter, ord)

size :: Char -> Int
size char = ord char - ord 'A'

pad :: Int -> String
pad = (`replicate` ' ')

line :: Char -> Int -> Int -> String
line _    x 0 = pad x ++ "A" ++ pad x
line char x y = pad x ++ [char] ++ pad y ++ [char] ++ pad x

prev :: Char -> Char
prev char = chr (ord char - 1)

half :: Char -> Char -> [String]
half orig 'A'  = [line orig (size orig) 0]
half orig char = half orig (prev char) ++ [newline]
  where newline = line char (size orig - size char) ((size char * 2) - 1)

diamond :: Char -> Maybe [String]
diamond c
  | isLetter c = Just $ half c c ++ (tail . reverse) (half c c)
  | otherwise  = Nothing
