module Raindrops (convert) where

import Data.Maybe (fromMaybe)
import Data.Monoid

convert :: Int -> String
convert n = fromMaybe (show n) sounds
  where sounds = sound 3 "Pling" <> sound 5 "Plang" <> sound 7 "Plong"
        sound factor noise
          | n `rem` factor == 0 = Just noise
          | otherwise           = Nothing
