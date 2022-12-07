module Lib
    ( solvers
    , solutions
    , solutionsTable
    ) where

import Solver

import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Days.Day5
import Days.Day6

solvers :: [Solver]
solvers = [day1, day2, day3, day4, day5, day6]

solutions :: IO [Solution]
solutions = sequence $ runSolver <$> solvers

solutionsTable :: IO String
solutionsTable = genTable <$> solutions