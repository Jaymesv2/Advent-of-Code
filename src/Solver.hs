{-# LANGUAGE GADTs, RankNTypes #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    , genTable
    ) where

import Text.Printf
import Util
import Data.List (unzip4)

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
genTable sols = makeTableWithHeaders ["Day", "Name", "Part 1", "Part 2"] [show <$> a,b,c,d]
    where (a,b,c,d) = unzip4 $ fmap solutionToTuple sols

data BTree a = Branch (BTree a) (BTree a) | Leaf a

revBTree :: BTree a -> BTree a
revBTree (Branch a b) = Branch b a
revBTree (Leaf a) = Leaf a