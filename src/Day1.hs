module Day1 (day1) where

import Util
import Solver

import Data.List (sort)
import Control.Arrow

day1 :: Solver
day1 = mkSolver 1 $ (head &&& sum . take 3) . reverse . sort . sumGroups
    where sumGroups = fmap (sum . fmap read) . seperateGroups
          seperateGroups = splitOn (==[]) . lines