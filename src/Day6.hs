module Day6(day6) where

import Solver
import Util

import Control.Arrow

uniqueElems :: (Eq a) => [a] -> Bool
uniqueElems [] = True
uniqueElems (x:xs) = x `notElem` xs && uniqueElems xs

firstUniqueNLength :: Int -> String -> Int
firstUniqueNLength n = (+n) . fst . head . filter (uniqueElems . snd) . windowsN n 

day6 :: Solver
day6 = mkSolver 6 $ (firstUniqueNLength 4 &&& firstUniqueNLength 14)