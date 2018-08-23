module ETL (transform) where

import Data.Char (toLower)
import Data.Map.Strict (Map, alter, empty, foldlWithKey)

add :: Int -> Maybe Int -> Maybe Int
add k = Just . maybe k (k +)

transform' :: Map Char Int -> Int -> String -> Map Char Int
transform' mm key = foldl (flip (alter (add key))) mm . map toLower

transform :: Map Int String -> Map Char Int
transform = foldlWithKey transform' empty
