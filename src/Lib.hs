module Lib
    ( solvers
    , solutions
    , solutionsTable
    , latestSolution
    , latestSolutionTable
    , solutionN
    , solutionNTable
    ) where

import Solver

import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6
import Days.Day7

solvers :: [Solver]
solvers = [day1, day2, day3, day4, day5, day6, day7]

solutions :: IO [Solution]
solutions = sequence $ runSolver <$> solvers

solutionN :: Int -> IO Solution
solutionN n = runSolver (solvers !! n) 

latestSolution :: IO Solution
latestSolution = runSolver $ last solvers

solutionsTable :: IO String
solutionsTable = genTable <$> solutions

solutionNTable :: Int -> IO String
solutionNTable n = genTable . (:[]) <$> solutionN n

latestSolutionTable :: IO String
latestSolutionTable = solutionNTable (length solvers - 1)
