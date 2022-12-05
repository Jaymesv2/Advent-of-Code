module Day2 (day2) where

import Data.Char
import Control.Arrow 

setPlay :: (Int, Int) -> (Int, Int)
setPlay (x,y) = (x, (x+[2, 0, 1]!!y)`mod`3)

score :: (Int, Int) -> Int
score (x,y) = (y+1)+([3,0,6]!!((x-y)`mod`3))

day2 :: IO (Int, Int)
day2 = do
    cont <- lines <$> readFile "inputs/day2-1.txt"
    let rounds = (abs . (-) 65 . (!! 0) &&& abs . (-) 88 . (!! 2)) . fmap ord <$> cont
    pure (sum . fmap score $ rounds, sum . fmap (score . setPlay) $ rounds)