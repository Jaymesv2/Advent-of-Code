module Days.Day3 (day3) where

import Util
import Solver

import Data.Char

day3 :: Solver
day3 = mkSolver 3 "Rucksack Reorganization" $ (\s -> 
    let f = sum . fmap (priorityOf . head)
        cont = lines s
    in (f $ uncurry union . compartments <$> cont,f $ groupIntersection <$> groupsOf 3 cont))

compartments :: [a] -> ([a], [a])
compartments r = splitAt (length r `div` 2) r 

priorityOf :: Char -> Int
priorityOf c = ord c - if isUpper c then 38 else 96

groupIntersection :: Ord a => [[a]] -> [a]
groupIntersection [] = []
groupIntersection [x] = x
groupIntersection (x:xs) = x `union` groupIntersection xs
