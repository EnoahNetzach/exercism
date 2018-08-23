{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts) where

import Data.Map.Strict (Map, findWithDefault, fromDistinctAscList, fromListWith)

bases :: String
bases = "ACGT"

validBase :: Char -> Either String Char
validBase base
  | base `elem` bases = Right base
  | otherwise         = Left $ "invalid base \"" ++ show base ++ "\""

validStrand :: String -> Either String String
validStrand = mapM validBase

countBases :: String -> Map Char Int
countBases = fromListWith (+) . flip zip (repeat 1)

occurWithDefault :: String -> Char -> Either String Int
occurWithDefault strand x = findWithDefault 0 x . countBases <$> validStrand strand

baseOccur :: String -> Char -> Either String (Char, Int)
baseOccur strand x = (x,) <$> occurWithDefault strand x

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts strand = fromDistinctAscList <$> mapM (baseOccur strand) bases
