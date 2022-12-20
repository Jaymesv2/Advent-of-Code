module Days.Day14(day14) where

import Solver
import Util.List (tupleWindows2)

import Control.Applicative
import Control.Arrow
import Data.Set as S hiding (foldl')
import Data.List (foldl', find)
import Text.Parsec hiding (between)
import Text.Parsec.Text

day14 :: Solver
day14 = mkParsecSolver 14 "Regolith Reservoir" parseInput $ (first (\m y -> snd y >= m+1) >>> sandSim) &&& (insertFloor >>> sandSim >>> (+1))

type Cordinate = (Int, Int)
type Rock = [Cordinate]

parseInput :: Parser (Int, Set Cordinate)
parseInput = makeRockSet <$> sepEndBy1 rockP (string "\n")
    where rockP = sepBy1 cordP $ string " -> "
          cordP = liftA2 (,) (numP <* char ',') numP
          numP = read <$> many1 digit

between :: (Cordinate, Cordinate) -> Set Cordinate -- all cords between 2 points
between ((x1,y1), (x2,y2)) = fromList [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]

insertFloor :: (Int, Set Cordinate) -> (Cordinate -> Bool, Set Cordinate)
insertFloor (m, rocks) = (const False, union rocks $ fromList [(x, m+2) | x <- [0..1000]])

makeRockSet :: [Rock] -> (Int, Set Cordinate)
makeRockSet = foldl' (\(m, s) rock -> (max m $ maximum (snd <$> rock), unions $ s:(between <$> tupleWindows2 rock))) (0, S.empty)

sandSim :: (Cordinate -> Bool, Set Cordinate) -> Int
sandSim (stopCond, rocks) = size $ sandSim' rocks `difference` rocks
    where
        sandSim' :: Set Cordinate -> Set Cordinate
        sandSim' rocks' = if stopCond nextSand || nextSand == (500,0) then rocks' else sandSim' (insert nextSand rocks')
            where nextSand = sandTick rocks' (500,0)
        sandTick :: Set Cordinate -> Cordinate -> Cordinate -- spawn one sand and use sandTick' to move it down
        sandTick rocks' rock@(x,y) = if not $ stopCond rock
            then case find (`notMember` rocks') [(x,y+1), (x-1,y+1), (x+1,y+1)] of
                Just cord -> sandTick rocks' cord
                _ -> rock
            else rock
{-
visualizeRocks :: Cordinate -> Int -> Set Cordinate -> String
visualizeRocks center s rocks = intercalate "\n" $ generateGrid center s (`member` rocks)

generateGrid :: Cordinate -> Int -> (Cordinate -> Bool) -> [String]
generateGrid c s check = generateGrid' c s (\x -> if check x then '#' else '.')

generateGrid' :: Cordinate -> Int -> (Cordinate -> Char) -> [String]
generateGrid' (cx, cy) radius check = [[check (x,y) | x <- [cx-radius..cx+radius]] | y <- [cy-radius..cy+radius]] ++ ["\n"]
-}