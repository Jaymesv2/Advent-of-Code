module Day4(day4) where

import Control.Arrow

day4 :: IO (Int, Int)
day4 = do
    cont <- lines <$> readFile "inputs/day4-1.txt"
    let calcAnsw f = length $ filter f $ parseInput <$> cont
    pure (calcAnsw completeOverlap,calcAnsw anyOverlap)

completeOverlap :: ((Int, Int), (Int,Int)) -> Bool
completeOverlap ((a,b), (x,y)) = ((a <= x && b >= y) || (x <= a && y >= b))

anyOverlap :: ((Int, Int), (Int,Int)) -> Bool
anyOverlap ((a,b), (x,y)) = (b >= x) && (y >= a)

parseInput :: String -> ((Int, Int), (Int,Int))
parseInput = (f *** f) . second tail . break (==',') 
    where f = (read *** read) . second tail . break (=='-') 
