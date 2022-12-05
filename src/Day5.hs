module Day5(day5) where

import Lib (splitOn)
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List (transpose, foldl')
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (liftM3)

day5 :: IO (String, String)
day5 = do
    cont <- lines <$> readFile "inputs/day5-1.txt"
    let instructions = parseInstructions $ last $ splitOn (==[]) cont
        initialState = filter (not . null) $ parseStacks $ init $ head $ splitOn (==[]) cont
        finalState = foldl' applyInstruction initialState instructions
        finalState2 = foldl' applyInstruction2 initialState instructions
    pure (fmap head $ finalState,fmap head $ finalState2)

applyInstruction, applyInstruction2 :: [[a]] -> (Int, Int, Int) -> [[a]]
-- moves elements one at a time
applyInstruction stacks (n, from', to') = (stacks & ix (from'-1) %~ drop n) & ix (to'-1) %~ ((++) $ reverse $ take n $ stacks !! (from'-1))

-- moves multiple elements at a time
applyInstruction2 stacks (n, from', to') = (stacks & ix (from'-1) %~ drop n) & ix (to'-1) %~ ((++) $ take n $ stacks !! (from'-1))

parseStacks :: [String] -> [[Char]]
parseStacks = fmap (filter (/=chr 0)) . transpose .  fmap (fst . last . readP_to_S parseLine)
    where parseCell = (char '[' *> get <* char ']') <|> (string "   " *> pure (chr 0))
          parseLine = sepBy parseCell (char ' ')

parseInstructions :: [String] -> [(Int, Int, Int)]
parseInstructions =  fmap $ fst . last . readP_to_S parseInstruction
    where 
        parseInstruction = liftM3 (,,) (f "move ") (f " from ") (f" to ")
        f s = string s *> (read <$> munch1 isDigit)