{-# LANGUAGE GADTs, RankNTypes #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    , genTable
    ) where

import Text.Printf
import Data.List (intercalate, unzip4, transpose)

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
genTable sols = makeTableWithHeaders [show <$> a,b,c,d] ["Day", "Name", "Part 1", "Part 2"]
    where (a,b,c,d) = unzip4 $ fmap solutionToTuple sols

makeTableWithHeaders :: [[String]] -> [String] -> String
makeTableWithHeaders columns headers = makeTable $ zipWith (:) headers columns


--makeTable :: Show a => [[a]] -> String
makeTable :: [[String]] -> String
makeTable columns = genSep columnWidths "┌" "┬" "┐\n" ++ intercalate (genSep columnWidths "├" "┼" "┤\n") rows ++ genSep columnWidths "└" "┴" "┘\n"
    where
        columnWidths = fmap (maximum . fmap length) columns
        rows = (flip formatRow) columnWidths <$> transpose columns

        padTo :: Int -> String -> String
        padTo len s = uncurry (++) $ (s++) <$> splitAt (n `quot` 2) (replicate n ' ')
            where n = len - length s
        
        genSep :: [Int] -> String -> String -> String -> String
        genSep widths beg sep end = beg ++ (intercalate sep $ flip replicate '─'  <$> widths) ++ end

        formatRow :: [String] -> [Int] -> String
        formatRow row widths =  "│" ++ (intercalate "│" $ uncurry padTo <$> x) ++ "│\n"
            where x = zip widths row
