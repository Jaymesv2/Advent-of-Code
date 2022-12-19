module Days.Day6(day6) where

import Solver
import Util.List

import Control.Arrow
import Data.List (findIndices)

uniqueElems :: (Eq a) => [a] -> Bool
uniqueElems [] = True
uniqueElems (x:xs) = x `notElem` xs && uniqueElems xs

firstUniqueNLength :: Int -> String -> Int
firstUniqueNLength n = (+(n+1)) . head . findIndices uniqueElems . windows n

day6 :: Solver
day6 = mkSolver 6 "Tuning Trouble" $ firstUniqueNLength 4 &&& firstUniqueNLength 14