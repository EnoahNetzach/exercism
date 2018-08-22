module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance s1 s2
  | length s1 /= length s2 = Nothing
  | otherwise              = Just $ length $ filter (uncurry (/=)) (zip s1 s2)
