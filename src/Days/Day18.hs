{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module Days.Day18(day18) where

import Solver

import Control.Applicative
import Text.Parsec.Text
import Text.Parsec
import qualified Data.Set as S
import qualified Data.Sequence as Se
import Data.List (foldl', partition)

type Cordinate = (Int,Int,Int)

parseInput :: Parser [Cordinate]
parseInput = sepBy1 cordP endOfLine
    where
        cordP = liftA3 (,,) (numP <* char ',') (numP <* char ',') numP
        numP = read <$> many1 digit

maxCords :: Cordinate -> Cordinate -> Cordinate
maxCords (maxx, maxy, maxz) (x,y,z) = (max x maxx, max y maxy, max z maxz)

neighbors :: Cordinate -> [Cordinate]
neighbors (x,y,z) = [(x,y,z+1),(x,y,z-1),(x,y+1,z),(x,y-1,z),(x+1,y,z),(x-1,y,z)]

neighborsInRange :: Cordinate -> Cordinate -> [Cordinate]
neighborsInRange (mx,my,mz) (x,y,z) = filter (\(x',y',z') -> inRange x' mx && inRange y' my && inRange z' mz) [(x,y,z+1),(x,y,z-1),(x,y+1,z),(x,y-1,z),(x+1,y,z),(x-1,y,z)]
    where inRange a r = -1 <= a && a <= r+1

findReachable :: (Ord s ) => (s -> [s]) -> (s -> Bool) -> s -> [(s,s)]
findReachable neighborsOf isLava start = S.toList $ (\(_,_,x) -> x) $ searcherM (Se.singleton start, S.empty, S.empty)
    where
        searcherM (cur Se.:<| hs, been, out) = if cur `S.notMember` been 
            then searcherM (hs Se.>< Se.fromList air, S.insert cur been, S.union out $ S.fromList ((cur,) <$> lava))
            else searcherM (hs, been, out) 
                where (lava, air) = partition isLava (neighborsOf cur)
        searcherM x = x

day18' :: [Cordinate] -> (Int, Int)
day18' [] = (0,0)
day18' cords@(c:cs) = (part1, part2)
    where 
        part1 = sum $ length . filter (`S.notMember` S.fromList cords) . neighbors <$> cords
        part2 = length $ findReachable (neighborsInRange $ foldl' maxCords c cs) (`S.member` S.fromList cords) (0,0,0)

day18 :: Solver
day18 = mkParsecSolver 18 "Boiling Boulders" parseInput day18'