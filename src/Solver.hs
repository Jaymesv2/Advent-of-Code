{-# LANGUAGE GADTs, RankNTypes #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    ) where

import Text.Printf

data Solver = Solver Int (String -> Solution)

data Solution = forall a b. (Show a, Show b) => Solution Int a b

instance Show Solution where
    show (Solution n x y) = printf "Day %d solution: part 1: %s, part 2: %s" n (show x) (show y)

mkSolver :: (Show a, Show b) => Int -> (String -> (a,b)) -> Solver
mkSolver n s = Solver n (uncurry (Solution n) . s)

runSolverPure :: String -> Solver -> Solution
runSolverPure i (Solver _ s) = s i

runSolver' ::  (Int -> FilePath) -> Solver -> IO Solution
runSolver' inputFinder (Solver n solver)  = solver <$> (readFile $ inputFinder n)

runSolver :: Solver -> IO Solution
runSolver = runSolver' (printf "inputs/%02d.txt")
