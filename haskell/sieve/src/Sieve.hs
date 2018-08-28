module Sieve (primesUpTo) where

sieve :: [Integer] -> [Integer] -> [Integer]
sieve [] primes = primes
sieve (x:xs) primes = sieve (filter (\n -> n `mod` x /= 0) xs) (primes ++ [x])

primesUpTo :: Integer -> [Integer]
primesUpTo n = sieve [2..n] []
