module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | any (<= 0) [a, b, c] || s3 >= s1 + s2 = Illegal
  | a == b && b == c                   = Equilateral
  | a == b || a == c || b == c         = Isosceles
  | otherwise                          = Scalene
    where [s1, s2, s3] = sort [a, b, c]
