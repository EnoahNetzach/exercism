module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n
  | n .&. 16 /= 0  = reverse $ handshake (n `xor` 16)
  | n .&. 8 /= 0   = handshake (n `xor` 8) ++ ["jump"]
  | n .&. 4 /= 0   = handshake (n `xor` 4) ++ ["close your eyes"]
  | n .&. 2 /= 0   = handshake (n `xor` 2) ++ ["double blink"]
  | n .&. 1 /= 0   = handshake (n `xor` 1) ++ ["wink"]
  | otherwise      = []
