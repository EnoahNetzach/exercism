module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List
import Data.List.Index
import Data.List.Split
import qualified Data.Map.Strict as M

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = M.Map String [String]

plant :: Char -> Plant
plant 'C' = Clover
plant 'G' = Grass
plant 'R' = Radishes
plant 'V' = Violets
plant p   = error $ "No plant " ++ [p]

garden :: [String] -> String -> Garden
garden students plants = M.fromList $ imap (\i student -> (student, cups i)) students
  where cups i = map ((!! i) . chunksOf 2) (splitOn "\n" plants)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = map plant (intercalate "" $ garden M.! student)
