module Luhn (isValid) where

import Data.Char

luhn :: [Int] -> [Int]
luhn = zipWith ($) (cycle [id, double]) . reverse
  where double n = if n < 5 then n * 2 else n * 2 - 9

checksum :: [Int] -> Int
checksum = (`mod` 10) . sum . luhn

isValid :: String -> Bool
isValid n = length digits > 1 && checksum digits == 0
  where digits = map digitToInt $ filter (/= ' ') n
