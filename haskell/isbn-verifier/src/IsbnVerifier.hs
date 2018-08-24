module IsbnVerifier (isbn) where

import Data.Char

sanitize :: String -> String
sanitize str = filter isDigit (init str) ++ filter check [last str]
  where check c = isDigit c || c == 'X'

convert :: String -> [Int]
convert = map convertX
  where convertX c
          | isDigit c = read (c:"")
          | c == 'X'  = 10
          | otherwise = 0

isbn :: String -> Bool
isbn str = length str >= 10 && length sanitized == 10 && result `mod` 11 == 0
  where sanitized = sanitize str
        converted = convert sanitized
        indicized = take (length converted) (zip [0..] converted)
        result = sum $ map (\(i, x) -> (10 - i) * x) indicized
