module Day6(day6) where

import Lib
import Control.Arrow

uniqueElems :: (Eq a) => [a] -> Bool
uniqueElems [] = True
uniqueElems (x:xs) = x `notElem` xs && uniqueElems xs

firstUniqueNLength :: Int -> String -> Int
firstUniqueNLength n = (+n) . fst . head . filter (uniqueElems . snd) . windowsN n 

day6 :: IO (Int, Int)
day6 = (firstUniqueNLength 4 &&& firstUniqueNLength 14) <$> readFile "inputs/day6-1.txt" 