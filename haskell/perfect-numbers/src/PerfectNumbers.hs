module PerfectNumbers (classify, Classification(..)) where

import Math.NumberTheory.Primes.Factorisation (factorise)

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = (if x `elem` xs then id else (x:)) $ uniq xs

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

aliquot :: Int -> Int
aliquot n =
  let factorised = factorise (toInteger n)
      factors = concatMap (\(x, t) -> replicate t (fromIntegral x)) factorised
      divisors = map product (uniq (init (subsets factors)))
  in sum divisors

classify :: Int -> Maybe Classification
classify n
  | n <= 0         = Nothing
  | aliquot n == n = Just Perfect
  | aliquot n > n  = Just Abundant
  | aliquot n < n  = Just Deficient
  | otherwise      = Nothing
