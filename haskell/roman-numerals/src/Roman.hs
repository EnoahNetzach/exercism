module Roman (numerals) where

data Tens = I | X | C | M deriving (Enum, Show)
data Fives = V | L | D deriving (Enum, Show)

tens :: Int -> Char
tens n = head $ show (toEnum n :: Tens)

fives :: Int -> Char
fives n = head $ show (toEnum n :: Fives)

numeral :: Int -> Int -> Int -> String
numeral 1 p t = replicate t (tens p)
numeral 4 p 1 = tens p : numeral 5 p 1
numeral 5 p 1 = fives p : ""
numeral 9 p 1 = tens p : numeral 1 (p + 1) 1
numeral _ _ _ = error "invalid numeral"

numeralPower :: Int -> Int -> String
numeralPower n p
  | n `elem` [4, 5, 9] = numeral n p 1
  | n < 4              = numeral 1 p n
  | otherwise          = numeral 5 p 1 ++ numeral 1 p (n - 5)

numeralsPower :: Int -> Int -> String
numeralsPower 0 _ = ""
numeralsPower m p = numeralPower n p ++ numeralsPower m' (p - 1)
  where
    d = fromIntegral m :: Double
    n = floor (d / 10 ^ p)
    m' = m - n * 10 ^ p

numerals :: Integer -> Maybe String
numerals n
  | n < 5000  = Just $ numeralsPower (fromIntegral n) 3
  | otherwise = Nothing
