module Bob (responseFor) where

import Data.Char
import Data.List
import qualified Data.Text as T

responseFor :: String -> String
responseFor xs
  | question && shouted = "Calm down, I know what I'm doing!"
  | question            = "Sure."
  | shouted             = "Whoa, chill out!"
  | empty               = "Fine. Be that way!"
  | otherwise           = "Whatever."
  where
    trimmed = (T.unpack . T.strip . T.pack) xs
    question = not (null trimmed) && last trimmed == '?'
    shouted = not (null $ find isAlpha xs) && null (find (\c -> isAlpha c && isLower c) xs)
    empty = not $ any (not . isSpace) xs


