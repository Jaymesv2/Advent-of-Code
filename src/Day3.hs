module Day3 (day3) where

import Util
import Solver

import Data.Char

day3 :: Solver
day3 = mkSolver 3 "Rucksack Reorganization" $ (\s -> 
    let cont = lines s
        f = sum . fmap (priorityOf . head)
        a = f $ uncurry union . compartments <$> cont
        b = f $ groupIntersection <$> groupsOf 3 cont    
    in (a,b))

compartments :: [a] -> ([a], [a])
compartments r = (take n r, drop n r)
    where n = length r `div` 2

priorityOf :: Char -> Int
priorityOf c = ord c - if isUpper c then 38 else 96

groupIntersection :: Ord a => [[a]] -> [a]
groupIntersection [] = []
groupIntersection [x] = x
groupIntersection (x:xs) = x `union` groupIntersection xs
