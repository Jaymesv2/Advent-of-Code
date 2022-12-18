module Days.Day12(day12) where

import Solver

import Data.Char

import Control.Arrow
import Data.List (elemIndices)
import Algorithm.Search

type Matrix a = [[a]]
type Key = (Int, Int)

day12 :: Solver
day12 = mkSolver 12 "Hill Climbing Algorithm" $ lines >>> (part1 &&& part2)

part1, part2 :: Matrix Char -> Int
part1 = solve (allXPos 'S' >>> head) (allXPos 'E') (\x y -> x - y >= -1)
part2 = solve (allXPos 'E' >>> head) (allXPos 'a') (\x y -> y - x >= -1)
 
allXPos :: Char -> Matrix Char -> [Key]
allXPos x matrix = concat $ zipWith (zip . repeat) [0 ..] $ fmap (elemIndices x) matrix

solve :: (Matrix Char -> Key) -> (Matrix Char -> [Key]) -> (Int -> Int -> Bool) -> Matrix Char -> Int
solve startF endF canMove matrix = length $ maybe [] snd $ dijkstra getNeighbors (\_ _ -> 1) (flip elem $ endF matrix) $ startF matrix
    where lm = length matrix
          ln = length (head matrix)
          getNeighbors :: Key -> [Key]
          getNeighbors (m,n) = filter (\(x, y) -> x >= 0 && x < lm && y >= 0 && y < ln && canMove (mAt (m,n)) (mAt (x,y))) [(m+1, n), (m-1, n), (m, n+1), (m, n-1)]
          mAt :: Key -> Int
          mAt (m,n) = case (matrix !! m) !! n of 
            'S' -> 0
            'E' -> 26
            c   -> ord c - 97