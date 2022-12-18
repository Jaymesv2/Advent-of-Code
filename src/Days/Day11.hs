{-# LANGUAGE TupleSections #-}
module Days.Day11 (day11) where

import Solver

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.ST

import Data.Array.ST
import Data.Array
import Data.Char
--import Data.List (sort)

import Text.ParserCombinators.ReadP

day11 :: Solver
day11 = mkSolver 11 "Monkey in the Middle" $ parseInput >>> (monkeySim 20 &&& monkeySim 10000)

--                   inspect     test toT toF
data Monkey = Monkey (Int -> Int) Int Int Int
data MonkeySim s = MonkeySim (Array Int Monkey) (STArray s Int ([Int], Int))

parseInput :: String -> [(Monkey, [Int])]
parseInput = readP_to_S (sepBy parseMonkey $ string "\n\n") >>> last >>> fst

parseMonkey :: ReadP (Monkey, [Int])
parseMonkey = let numP = (read <$> many1 (satisfy isDigit)) in do 
    items <- string "Monkey " *> numP *> string ":\n  Starting items: " *> sepBy numP (string ", ")
    o <- string "\n  Operation: new = old " *> get <* char ' '
    s <- string "old" <|> many1 (satisfy isDigit)
    t <- string "\n  Test: divisible by " *> numP
    toT <- string "\n    If true: throw to monkey " *> numP
    toF <- string "\n    If false: throw to monkey " *> numP
    let inspect x  = (if o == '*' then (*) else (+)) x $ if s == "old" then x else read s
    pure (Monkey inspect t toT toF, items)

--monkeySim :: Int -> [(Monkey, [Int])] -> Int
monkeySim :: Int -> [(Monkey, [Int])] -> [Int]
monkeySim rounds inp = runST $ do
    sim@(MonkeySim m state) <- makeSim inp
    replicateM_ rounds (mapM_ (runMonkey sim) (indices m))
    fmap snd <$> getElems state
    --(product . take 2 . reverse . sort . fmap snd) <$> getElems state

makeSim :: [(Monkey, [Int])] -> ST s (MonkeySim s)
makeSim inp = MonkeySim (listArray (0, length inp-1) monkeys) <$> newListArray (0, length inp-1) ((,0) <$> items)
        where (monkeys, items) = unzip inp
    
runMonkey :: MonkeySim s -> Int -> ST s ()
runMonkey sim@(MonkeySim monkeys state) mId = do
    (items, c) <- readArray state mId 
    forM_ items (runMonkey' sim (monkeys ! mId)) 
    writeArray state mId ([], c+length items)

runMonkey' :: MonkeySim s -> Monkey -> Int -> ST s ()
runMonkey' (MonkeySim _ state) (Monkey operation test toT toF) x = do 
    (ys, c) <- readArray state whereTo
    writeArray state whereTo (ys ++ [newX], c)
        where newX = operation x `div` 3
              whereTo = if newX `mod` test == 0 then toT else toF
{-
runMonkey' :: MonkeySim s -> Monkey -> Int -> ST s ()
runMonkey' (MonkeySim monkeys state) (Monkey operation test toT toF) x = do 
    (ys, c) <- readArray state whereTo
    writeArray state whereTo (ys ++ [newX], c)
        --where newX = (operation x) `mod` (calcMod monkeys)
        where newX = (operation x)
              whereTo = if newX `mod` test == 0 then toT else toF

calcMod :: Array Int Monkey -> Int
calcMod = elems >>> fmap f >>> sum
    where f (Monkey _ i _ _) = i
-}