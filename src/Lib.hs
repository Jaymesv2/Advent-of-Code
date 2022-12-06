module Lib
    ( solvers
    , solutions
    ) where

import Solver

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

solvers :: [Solver]
solvers = [day1, day2, day3, day4, day5, day6]

solutions :: IO [Solution]
solutions = sequence $ runSolver <$> solvers
