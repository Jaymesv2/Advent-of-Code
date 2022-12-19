{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, OverloadedLists, UndecidableInstances #-}
module Solver 
    ( Solution
    , Solver
    , mkSolver
    , runSolver
    , genTable
    , solutionNum
    , mkParsecSolver
    ) where

import Text.Printf
import Util.List
import Data.List (unzip4)
import Data.Text
import Text.Parsec (Parsec, runParser)

data Solver = Solver Int String (String -> Solution)
data Solution = forall a b. (Show a, Show b) => Solution Int String a b

runPar :: Parsec Text () c -> String -> Text -> c
runPar parser name input = case runParser parser () name input of 
    Right a -> a
    Left err -> error (show err)

mkParsecSolver :: (Show a, Show b) => Int -> String -> Parsec Text () c -> (c -> (a,b)) -> Solver
mkParsecSolver day name parser s = Solver day name (uncurry (Solution day name) . s . runPar parser name . pack)

mkSolver :: (Show a, Show b) => Int -> String -> (String -> (a,b)) -> Solver
mkSolver d n s = Solver d n (uncurry (Solution d n) . s)

instance Show Solution where
    show (Solution day name p1 p2) = "Day " ++ show day ++ " \"" ++ name ++ "\", part1: " ++ show p1 ++ ", part2: " ++ show p2

solutionNum :: Solution -> Int
solutionNum (Solution n _ _ _) = n

solutionToTuple :: Solution -> (Int, String, String, String)
solutionToTuple (Solution d name x y) = (d, name, show x, show y)

runSolver' :: (Int -> FilePath) -> Solver -> IO Solution
runSolver' inputFinder (Solver n _ solver) = solver <$> readFile (inputFinder n)

runSolver :: Solver -> IO Solution
runSolver = runSolver' (printf "inputs/%02d.txt")

genTable :: [Solution] -> String
genTable sols = makeTableWithHeaders ["Day", "Name", "Part 1", "Part 2"] [show <$> a,b,c,d]
    where (a,b,c,d) = unzip4 $ fmap solutionToTuple sols