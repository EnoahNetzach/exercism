module TwelveDays (recite) where

import Data.List

day :: Int -> String
day  1 = "first"
day  2 = "second"
day  3 = "third"
day  4 = "fourth"
day  5 = "fifth"
day  6 = "sixth"
day  7 = "seventh"
day  8 = "eighth"
day  9 = "ninth"
day 10 = "tenth"
day 11 = "eleventh"
day 12 = "twelfth"
day  _ = error "We have only twelve days, I'm afraid..."

gift :: Int -> String
gift  1 = "a Partridge in a Pear Tree"
gift  2 = "two Turtle Doves"
gift  3 = "three French Hens"
gift  4 = "four Calling Birds"
gift  5 = "five Gold Rings"
gift  6 = "six Geese-a-Laying"
gift  7 = "seven Swans-a-Swimming"
gift  8 = "eight Maids-a-Milking"
gift  9 = "nine Ladies Dancing"
gift 10 = "ten Lords-a-Leaping"
gift 11 = "eleven Pipers Piping"
gift 12 = "twelve Drummers Drumming"
gift  _ = error "We have only twelve gifts, I'm afraid..."

line :: Int -> String
line num = incipit ++ refrain ++ "."
  where incipit = "On the " ++ day num ++ " day of Christmas my true love gave to me, "
        gifts = map gift (reverse [1..num])
        refrain = if length gifts == 1
          then head gifts
          else intercalate ", " (init gifts ++ ["and " ++ last gifts])

recite :: Int -> Int -> [String]
recite start stop = map line [start..stop]