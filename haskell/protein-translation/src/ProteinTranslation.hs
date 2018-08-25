module ProteinTranslation(proteins) where

import Data.Maybe

protein :: String -> Maybe String
protein codon
  | codon == "AUG"                            = Just "Methionine"
  | codon `elem` ["UUU", "UUC"]               = Just "Phenylalanine"
  | codon `elem` ["UUA", "UUG"]               = Just "Leucine"
  | codon `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
  | codon `elem` ["UAU", "UAC"]               = Just "Tyrosine"
  | codon `elem` ["UGU", "UGC"]               = Just "Cysteine"
  | codon == "UGG"                            = Just "Tryptophan"
  | codon `elem` ["UGU", "UGC"]               = Just "Cysteine"
  | otherwise                                 = Nothing

proteins :: String -> Maybe [String]
proteins rna = Just $ case protein (take 3 rna) of
  Just p  -> p : fromJust (proteins $ drop 3 rna)
  Nothing -> []

