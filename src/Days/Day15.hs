module Days.Day15 (day15) where

import Solver

import Control.Arrow
import Control.Applicative
import Text.Parsec
import Text.Parsec.Text
import Data.List (sortBy, foldl')

type Cordinate = (Int, Int)

day15 :: Solver
day15 = mkParsecSolver 15 "Beacon Exclusion Zone" parseInput (part1 &&& const "unimplemented")

part1 :: [(Cordinate, Cordinate)] -> Int
part1 beacons = length (filter (\x -> any (\(c, b) -> inRangeOf c b (x,y)) beaconDists) [mi..mx])-1
    where 
        y = 2000000
        beaconDists :: [(Cordinate, Int)]
        beaconDists = makeDistChecker <$> sortBy (\((x,_),_) ((y2,_),_) -> compare x y2) beacons
        (mi,mx) = foldl' (\(a1, b1) (a2, b2) -> (min a1 a2, max b1 b2)) (maxBound,minBound) intersections
        intersections = sortBy (\(a,_) (b,_)-> compare a b) $ intersectionsOnLine y <$> beaconDists

parseInput :: Parser [(Cordinate, Cordinate)]
parseInput = sepEndBy1 beaconP $ string "\n"
    where xyP = liftA2 (,) (string "x=" *> numP) (string ", y=" *> numP)
          beaconP = liftA2 (,) (string "Sensor at " *> xyP) (string ": closest beacon is at " *> xyP)
          numP = read <$> liftA2 (++) (option "" $ string "-") (many1 digit)

intersectionsOnLine :: Int -> (Cordinate, Int) -> Cordinate
intersectionsOnLine y ((bx, by), dist) = if a < b then (a,b) else (b,a) 
    where (a, b) = ((bx-) &&& (bx+)) $ dist - abs (by - y)

distance :: Cordinate -> Cordinate -> Int
distance (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

makeDistChecker :: (Cordinate, Cordinate) -> (Cordinate, Int)
makeDistChecker (beacon, sensor) = (beacon, distance beacon sensor)

inRangeOf :: Cordinate -> Int -> Cordinate -> Bool
inRangeOf base radius other = distance base other <= radius

{-
sampleInput :: String
sampleInput = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"

makeLine :: Int -> [(Cordinate, Int)] -> [Int]
makeLine y xs = [mi..mx]
    where (mi,mx) =(\x -> traceShow x x) $ foldl' (\(a1, b1) (a2, b2) -> (min a1 a2, max b1 b2)) (maxBound,0) (intersectionsOnLine y <$> xs)

inRangeOfAny' :: [(Cordinate, Int)] -> Cordinate -> Char
inRangeOfAny' beacons cord
    | cord `elem` fmap fst beacons = 'B'
    | any ((==) cord . (,y)) line = '-'
    | any (\(c,b) -> inRangeOf c b cord) beacons = '#'
    | otherwise = '.'
    where 
        line = makeLine y beacons
        y = 10

traceState :: [(Cordinate, Int)] -> [(Cordinate, Int)]
traceState x = trace (intercalate "\n" $ generateGrid' (0,0) 40 (inRangeOfAny' x)) x

generateGrid :: Cordinate -> Int -> (Cordinate -> Bool) -> [String]
generateGrid c s check = generateGrid' c s (\x -> if check x then '#' else '.')

generateGrid' :: Cordinate -> Int -> (Cordinate -> Char) -> [String]
generateGrid' (cx, cy) radius check = [[check (x,y) | x <- [cx-radius..cx+radius]] | y <- [cy-radius..cy+radius]] ++ ["\n"]
-}