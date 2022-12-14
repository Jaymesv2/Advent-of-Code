module Days.Day13(day13) where

import Solver

import Control.Applicative (liftA2)
import Control.Arrow
import Data.List (sortBy, findIndices)
import Text.Parsec
import Text.Parsec.Text

data NestedList = List [NestedList] | Leaf Int deriving (Show)

day13 :: Solver
day13 = mkParsecSolver 13 "Distress Signal" parser (part1 &&& part2)

parser :: Parser [(NestedList, NestedList)]
parser = sepEndBy1 pairP $ endOfLine *> optional endOfLine
    where pairP = liftA2 (,) (lstP <* char '\n') lstP
          lstP = List <$> between (char '[') (char ']') (sepBy (numP <|> lstP) $ char ',' )
          numP = Leaf . read <$> many1 digit

part1, part2 :: [(NestedList, NestedList)] -> Int
part1 = findIndices ((==LT) . uncurry compareLst) >>> fmap (+1) >>> sum

part2 = ((List [List [Leaf 2]], List [List [Leaf 6]]):) >>> fmap (\(x,y) -> [x,y]) >>> concat >>> sortBy compareLst >>> findIndices findDividerPacket >>> fmap (+1) >>> product

findDividerPacket :: NestedList -> Bool
findDividerPacket (List [List [Leaf x]]) | x == 2 || x == 6 = True
findDividerPacket _ = False

compareLst :: NestedList -> NestedList -> Ordering
compareLst (List x) (Leaf y) = compareLst (List x) (List [Leaf y])
compareLst (Leaf x) (List y) = compareLst (List [Leaf x]) (List y)
compareLst (Leaf x) (Leaf y) = compare x y 
compareLst (List (x:xs)) (List (y:ys)) = case compareLst x y of
    LT -> LT
    EQ -> compareLst (List xs) (List ys)
    GT -> GT
compareLst (List []) (List [])  = EQ
compareLst (List []) (List _) = LT
compareLst (List _) (List []) = GT