module Day3 (day3) where

import Util
import Solver

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set


day3 :: Solver
day3 = mkSolver 3 $ (\s -> 
    let cont = lines s
        f = sum . fmap (priorityOf . head . Set.toList)
        a = f $ uncurry Set.intersection . compartments <$> cont
        b = f $ groupIntersection . fmap Set.fromList <$> groupsOf 3 cont    
    in (a,b))

compartments :: Ord a => [a] -> (Set a, Set a)
compartments r = (Set.fromList $ take n r, Set.fromList $ drop n r)
    where n = length r `div` 2

priorityOf :: Char -> Int
priorityOf c = ord c - if isUpper c then 38 else 96

groupIntersection :: Ord a => [Set a] -> Set a
groupIntersection [] = Set.empty
groupIntersection [x] = x
groupIntersection (x:xs) = x `Set.intersection` groupIntersection (xs)
