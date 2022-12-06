{-# LANGUAGE GADTs, RankNTypes #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    , genTable
    ) where

import Text.Printf
import Data.List (intercalate, unzip4, zip4)

data Solver = Solver Int String (String -> Solution)

data Solution = forall a b. (Show a, Show b) => Solution Int String a b

solutionToTuple :: Solution -> (Int, String, String, String)
solutionToTuple (Solution d name x y) = (d, name, show x, show y)

mkSolver :: (Show a, Show b) => Int -> String -> (String -> (a,b)) -> Solver
mkSolver d n s = Solver d n (uncurry (Solution d n) . s)

runSolver' :: (Int -> FilePath) -> Solver -> IO Solution
runSolver' inputFinder (Solver n _ solver) = solver <$> (readFile $ inputFinder n)

runSolver :: Solver -> IO Solution
runSolver = runSolver' (printf "inputs/%02d.txt")

genTable :: [Solution] -> String
genTable sols = genSep "┌" "┬" "┐\n" ++ intercalate (genSep "├" "┼" "┤\n") (formatRow <$> zip4 ns ds xs ys) ++ genSep "└" "┴" "┘\n"
    where
        (ln, ld, lx, ly) = (f ns, f ds, f xs, f ys)
        (ns, ds, xs, ys) = ("Day":fmap show ns', "Name":ds', "Part 1":xs', "Part 2":ys')
            where (ns', ds', xs', ys') = unzip4 $ fmap solutionToTuple sols

        f :: [String] -> Int
        f = maximum . fmap length

        pad :: Int -> String -> String
        pad len s = replicate front ' ' ++ s ++ replicate (neededPadding - front) ' '
            where neededPadding = len - length s
                  front = neededPadding `quot` 2

        genSep :: String -> String -> String -> String
        genSep beg sep end = beg ++ replicate (ln+2) '─' ++ sep ++ replicate (ld+2) '─' ++ sep ++ replicate (lx+2) '─' ++ sep ++ replicate (ly+2) '─' ++ end

        formatRow :: (String, String, String, String) -> String
        formatRow (n,d,x,y) = "│ " ++ pad ln n ++ " │ " ++ pad ld d  ++ " │ " ++ pad lx x ++ " │ " ++ pad ly y ++ " │\n"
