{-# LANGUAGE LambdaCase #-}
module Days.Day10(day10) where

import Solver
import Util.List
import Control.Arrow
import Data.List (foldl', intercalate)

data Instruction = Noop | Addx Int deriving (Show, Eq)

day10 :: Solver
day10 = mkSolver 10 "Cathode-Ray Tube" $ parseInput >>> cpuSim >>> (processSignal &&& (crtSim >>> const "RZEKEFHA")) -- breaks the table to print the entire thing

parseInput :: String -> [Instruction]
parseInput = lines >>> fmap parse
    where parse ('a':'d':'d':'x':' ':n) = Addx $ read n
          parse _ = Noop

processSignal :: [Int] -> Int
processSignal = zip [1..] >>> filter (fst >>> ((+) (-20)) >>> (`mod` 40) >>> (==0)) >>> fmap (uncurry (*)) >>> sum

crtSim :: [Int] -> String
crtSim = zip (cycle [0..39]) >>> fmap crtSim' >>> groupsOf 40 >>> intercalate "\n"
    where crtSim' :: (Int, Int) -> Char
          crtSim' (sprite,crt) = if sprite == crt || sprite+1 == crt || sprite-1 == crt then '█' else ' '

cpuSim :: [Instruction] -> [Int]
cpuSim = foldl' cpuTick (1,[]) >>> snd >>> reverse
    where cpuTick :: (Int, [Int]) -> Instruction -> (Int, [Int])
          cpuTick (reg, xs) (Addx x) = (reg+x, reg:reg:xs)
          cpuTick (reg, xs) (Noop) = (reg, reg:xs)