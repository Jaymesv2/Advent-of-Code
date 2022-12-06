module Day4(day4) where

import Solver
import Control.Arrow

day4 :: Solver
day4 = mkSolver 4 $ (calcAnsw completeOverlap &&& calcAnsw anyOverlap) . lines
        where calcAnsw f = length . (filter f) . fmap parseInput

completeOverlap :: ((Int, Int), (Int,Int)) -> Bool
--completeOverlap ((a,b), (x,y)) = ((x < a && y > b) || (a < x && b > y))
completeOverlap ((a,b), (x,y)) = ((a <= x && b >= y) || (x <= a && y >= b))

anyOverlap :: ((Int, Int), (Int,Int)) -> Bool
anyOverlap ((a,b), (x,y)) = (x < b) && (a < y)

parseInput :: String -> ((Int, Int), (Int,Int))
parseInput = (f *** f) . second tail . break (==',') 
    where f = (read *** read) . second tail . break (=='-') 
