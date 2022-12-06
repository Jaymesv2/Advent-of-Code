module Day2 (day2) where

import Solver

import Data.Char
import Control.Arrow 

setPlay :: (Int, Int) -> (Int, Int)
setPlay (x,y) = (x, (x+[2, 0, 1]!!y)`mod`3)

score :: (Int, Int) -> Int
score (x,y) = (y+1)+([3,0,6]!!((x-y)`mod`3))

day2 :: Solver
day2 = mkSolver 2 "Rock Paper Scissors" $ (sum . fmap score &&& sum . fmap (score . setPlay)) . rounds . lines 
        where rounds = fmap $ (abs . (-) 65 . (!! 0) &&& abs . (-) 88 . (!! 2)) . fmap ord