module Days.Day8 (day8) where

import Solver
import Data.Char
import Control.Arrow
import Data.List (transpose, foldl')

day8 :: Solver
day8 = mkSolver 8 "Treetop Tree House" $ (countVisibleTrees &&& maxScenicScore ) . parseInput

countVisibleTrees :: [[Int]] -> Int
countVisibleTrees treeMap = sum . fmap sum $ seenMap 
    where topShown = calcShownFromSide treeMap
          leftShown = rotl . calcShownFromSide . rotr $ treeMap
          rightShown = rotr .  calcShownFromSide . rotl $ treeMap
          bottomShown = mFlip . calcShownFromSide . mFlip $ treeMap
          seenMap = foldl' (zipWith (zipWith max)) topShown [leftShown, rightShown, bottomShown]

parseInput :: String -> [[Int]]
parseInput s = fmap ((+) (-48) . ord) <$> lines s 

rotl, rotr, mFlip :: [[x]] -> [[x]]
rotl = transpose . map reverse
rotr = map reverse . transpose
mFlip = rotl . rotl

maxScenicScore :: [[Int]] -> Int
maxScenicScore hMap = maximum (uncurry (scenicScore hMap) <$> [(x,y) | x <- [1..length hMap-2], y <- [1..length (head hMap)-2]])

-- makes 4 lists containing the trees looking in each direction, calculates how far into that list is visible and multiplies the values
scenicScore :: [[Int]] -> Int -> Int -> Int
scenicScore hMap row col = product $ viewDistance ((hMap !! row) !! col) <$> [l, r, u, d]
    where (l, r) = (reverse *** tail) $ splitAt col [(hMap !! row) !! x   | x <- [0..length (head hMap)-1]]
          (u, d) = (reverse *** tail) $ splitAt row [(hMap !! x  ) !! col | x <- [0..length hMap-1]]

viewDistance :: Int -> [Int] -> Int
viewDistance x row = if u < length row then u+1 else u where u = length $ takeWhile (<x) row 

calcShownFromSide :: [[Int]] -> [[Int]]
calcShownFromSide = fmap (markVisible (-1))

-- returns a new matrix which has 1 in the location of the seen trees and 0 in the location of the unseen trees, scans each row left to right
markVisible :: Int -> [Int] -> [Int]
markVisible startHeight row = reverse $ fst $ foldl' mark ([], startHeight) row
    where mark (marked, mx) n = (fromEnum (mx < n):marked, max mx n)