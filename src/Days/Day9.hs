module Days.Day9 (day9) where

import Solver
import Control.Arrow
import Data.List (foldl')
import qualified Data.Set as S

type Cordinate = (Int, Int)
type Rope = [Cordinate]
type RopeSimState = (Rope, S.Set Cordinate)
data Instruction = L | R | U | D deriving (Read, Show, Eq)

day9 :: Solver
day9 = mkSolver 9 "Rope Bridge" $ parseInput >>> (ropeSim (nLenRope 2) &&& ropeSim (nLenRope 10))

nLenRope :: Int -> Rope
nLenRope len = replicate len (0,0) 

parseInput :: String -> [Instruction]
parseInput = lines >>> fmap ((read . drop 2 &&& read . take 1) >>> uncurry replicate) >>> concat

ropeSim :: Rope -> [Instruction] -> Int
ropeSim rope = foldl' ropeSimTick (rope, S.empty) >>> snd >>> S.size

headTick :: Cordinate -> Instruction -> Cordinate
headTick (x,y) ins = case ins of
    L -> (x-1,y)
    R -> (x+1,y)
    U -> (x,y+1)
    D -> (x,y-1)

ropeSimTick :: RopeSimState -> Instruction -> RopeSimState
ropeSimTick ([], _) _ = error "invalid rope"
ropeSimTick ((h:t),s) ins = (newRope, S.insert (last newRope) s)
    where newRope = newHead:(tailTick newHead t)
          newHead = headTick h ins

tailTick :: Cordinate -> Rope -> Rope
tailTick _ [] = []
tailTick head' (x:xs) = n:tailTick n xs
    where n = tailTick' head' x
          tailTick' :: Cordinate -> Cordinate -> Cordinate
          tailTick' h@(hx, hy) t@(tx, ty) = if h `isTouching` t then t 
                    else (tx - fromEnum (tx > hx) + fromEnum (tx < hx), ty - fromEnum (ty > hy) + fromEnum (ty < hy))

isTouching :: Cordinate -> Cordinate -> Bool
isTouching (ax, ay) b = b == (ax+1, ay+1) || b == (ax+1, ay) || b == (ax+1, ay-1) 
                     || b == (ax  , ay+1) || b == (ax  , ay) || b == (ax  , ay-1)
                     || b == (ax-1, ay+1) || b == (ax-1, ay) || b == (ax-1, ay-1)