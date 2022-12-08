{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}
module Main (main) where

import Lib
import Solver
import Data.Typeable
import System.Console.CmdArgs

data AdventOfCode = Day {day :: Int}
          | Table
          | Latest
          deriving (Data,Typeable,Show,Eq)
          
day' = Day
    {day = def &= typ "Day" &= argPos 0
    } &= help "Print a Day"

table' = Table -- &= auto
latest' = Latest &= auto

main :: IO ()
main = cmdArgs (modes [day', table', latest']) >>= (\case
        Day {day = d} -> solutionNTable (d-1)
        Table -> solutionsTable
        Latest -> latestSolutionTable) >>= putStrLn