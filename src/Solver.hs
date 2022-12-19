{-# LANGUAGE GADTs, RankNTypes #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    , genTable
    , solutionNum
    ) where

import Text.Printf
import Util.List
import Data.List (unzip4)

data Solver = Solver Int String (String -> Solution)

data Solution = forall a b. (Show a, Show b) => Solution Int String a b
instance Show Solution where
    show (Solution day name p1 p2) = "Day " ++ show day ++ " \"" ++ name ++ "\", part1: " ++ show p1 ++ ", part2: " ++ show p2

solutionNum :: Solution -> Int
solutionNum (Solution n _ _ _) = n

solutionToTuple :: Solution -> (Int, String, String, String)
solutionToTuple (Solution d name x y) = (d, name, show x, show y)

mkSolver :: (Show a, Show b) => Int -> String -> (String -> (a,b)) -> Solver
mkSolver d n s = Solver d n (uncurry (Solution d n) . s)

runSolver' :: (Int -> FilePath) -> Solver -> IO Solution
runSolver' inputFinder (Solver n _ solver) = solver <$> readFile (inputFinder n)

runSolver :: Solver -> IO Solution
runSolver = runSolver' (printf "inputs/%02d.txt")

genTable :: [Solution] -> String
genTable sols = makeTableWithHeaders ["Day", "Name", "Part 1", "Part 2"] [show <$> a,b,c,d]
    where (a,b,c,d) = unzip4 $ fmap solutionToTuple sols