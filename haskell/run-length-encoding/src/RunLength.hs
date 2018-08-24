module RunLength (decode, encode) where

import Data.Char
import Data.Maybe
import Data.List

decode' :: (Maybe String, [(Int, Char)]) -> Char -> (Maybe String, [(Int, Char)])
decode' (Nothing, list) c
  | isDigit c = (Just [c], list)
  | otherwise = (Nothing, list ++ [(1, c)])
decode' (Just n, list) c
  | isDigit c = (Just (n ++ [c]), list)
  | otherwise = (Nothing, list ++ [(read n, c)])

encode' :: (String, [(Int, Char)]) -> Char -> (String, [(Int, Char)])
encode' (lastChar, list) c
  | [c] == lastChar = ([c], init list ++ [(fst (last list) + 1, c)])
  | otherwise       = ([c], list ++ [(1, c)])

convert :: ((a, [(Int, Char)]) -> Char -> (a, [(Int, Char)])) -> ((Int, Char) -> String) -> a -> String -> String
convert converter mapper start = intercalate "" . map mapper . snd . foldl converter (start, [])

decode :: String -> String
decode = convert decode' (uncurry replicate) Nothing

encode :: String -> String
encode = convert encode' chunk ""
  where chunk (i, c) = (if i > 1 then show i else "") ++ [c]
