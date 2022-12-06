{-# LANGUAGE GADTs, RankNTypes #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    , genTable
    ) where

import Text.Printf
import Data.List (intercalate)

data Solver = Solver Int (String -> Solution)

data Solution = forall a b. (Show a, Show b) => Solution Int a b

solutionToTuple :: Solution -> (Int, String, String)
solutionToTuple (Solution n x y) = (n, show x, show y)

mkSolver :: (Show a, Show b) => Int -> (String -> (a,b)) -> Solver
mkSolver n s = Solver n (uncurry (Solution n) . s)

runSolver' ::  (Int -> FilePath) -> Solver -> IO Solution
runSolver' inputFinder (Solver n solver)  = solver <$> (readFile $ inputFinder n)

runSolver :: Solver -> IO Solution
runSolver = runSolver' (printf "inputs/%02d.txt")

genTable :: [Solution] -> String
genTable sols = genSep "┌" "┬" "┐\n" ++ intercalate (genSep "├" "┼" "┤\n") (formatRow <$> zip3 ns xs ys) ++ genSep "└" "┴" "┘\n"
    where
        (ln, lx, ly) = (f ns, f xs, f ys)
        (ns, xs, ys) = ("Day":fmap show ns', "Part 1":xs', "Part 2":ys')
            where (ns', xs', ys') = unzip3 $ fmap solutionToTuple sols

        f :: [String] -> Int
        f = maximum . fmap length

        pad :: Int -> String -> String
        pad len s = replicate front ' ' ++ s ++ replicate (neededPadding - front) ' '
            where neededPadding = len - length s
                  front = neededPadding `quot` 2

        genSep :: String -> String -> String -> String
        genSep beg sep end = beg ++ replicate (ln+2) '─' ++ sep ++ replicate (lx+2) '─' ++ sep ++ replicate (ly+2) '─' ++ end

        formatRow :: (String, String, String) -> String
        formatRow (n,x,y) = "│ " ++ pad ln n ++ " │ " ++ pad lx x ++ " │ " ++ pad ly y ++ " │\n"
        