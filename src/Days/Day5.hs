module Days.Day5(day5) where

import Solver
import Util

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List (transpose, foldl')
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (liftM3)

day5 :: Solver
day5 = mkSolver 5 "Supply Stacks" day5'

day5' :: String -> (String, String)
day5' s = let cont = splitOn (==[]) $ lines s
              instructions = parseInstructions . last $ cont
              initialState = parseStacks . init . head $ cont
              eval mover = fmap head $ foldl' mover initialState instructions
          in (eval craneMover9000,eval craneMover9001)

type Instruction = (Int, Int, Int)
type State = [[Char]]

craneMover9000, craneMover9001 :: State -> Instruction -> State
-- moves elements one at a time
craneMover9000 stacks (n, from', to') = (stacks & ix (from'-1) %~ drop n) & ix (to'-1) %~ ((++) $ reverse $ take n $ stacks !! (from'-1))
-- moves multiple elements at a time
craneMover9001 stacks (n, from', to') = (stacks & ix (from'-1) %~ drop n) & ix (to'-1) %~ ((++) $ take n $ stacks !! (from'-1))

-- parses the cells out into a matrix containing the letter in the cell or a null byte if there was no cell. the transpose of the matrix with nulls filtered is returned
parseStacks :: [String] -> State
parseStacks = fmap (filter (/=chr 0)) . transpose .  fmap (fst . last . readP_to_S parseLine)
    where parseCell = (char '[' *> get <* char ']') <|> (string "   " *> pure (chr 0))
          parseLine = sepBy parseCell (char ' ')

parseInstructions :: [String] -> [Instruction]
parseInstructions =  fmap $ fst . last . readP_to_S parseInstruction
    where parseInstruction = liftM3 (,,) (f "move ") (f " from ") (f" to ")
          f s = string s *> (read <$> munch1 isDigit)