module Day3 (day3) where

import Data.Char
import Lib (groupsOf)

import Data.Set (Set)
import qualified Data.Set as Set


day3 :: IO (Int, Int)
day3 = do
    cont <- lines <$> readFile "inputs/day3-1.txt"
    
    let f = sum . fmap (priorityOf . head . Set.toList)
        a = f $ uncurry Set.intersection . compartments <$> cont
        b = f $ groupIntersection . fmap Set.fromList <$> groupsOf 3 cont    
    pure (a,b)

compartments :: Ord a => [a] -> (Set a, Set a)
compartments r = (Set.fromList $ take n r, Set.fromList $ drop n r)
    where n = length r `div` 2

priorityOf :: Char -> Int
priorityOf c = ord c - if isUpper c then 38 else 96

groupIntersection :: Ord a => [Set a] -> Set a
groupIntersection [] = Set.empty
groupIntersection [x] = x
groupIntersection (x:xs) = x `Set.intersection` groupIntersection (xs)
