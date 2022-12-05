module Day1 (day1) where

import Lib (splitOn)
import Data.List (sort)
import Control.Arrow

day1 :: IO (Int, Int)
day1 = (head &&& sum . take 3) . reverse . sort . fmap (sum . fmap read) . splitOn (==[]) . lines <$> readFile "inputs/day1-1.txt"