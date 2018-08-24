module Phone (number) where

import Data.List
import Data.Array
import Text.Regex.TDFA (Regex, MatchText, makeRegex, matchOnceText)

ptt :: String
ptt = "^" ++ country ++ spaces ++ area ++ symbols ++ local ++ spaces ++ "$"
  where spaces = " *"
        symbols = "[ .-]*"
        country = "\\+?1?"
        area = "\\(?([2-9][0-9]{2})\\)?"
        exchange = "([2-9][0-9]{2})"
        subscriber = "([0-9]{4})"
        local = exchange ++ symbols ++ subscriber

matcher :: String -> Maybe (String, MatchText String, String)
matcher = matchOnceText (makeRegex ptt :: Regex)

number :: String -> Maybe String
number n = case matcher n of
  Nothing -> Nothing
  Just (_, matches, _) -> Just $ intercalate "" $ map fst $ tail $ elems matches

