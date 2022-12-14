{-# LANGUAGE TupleSections #-}
module Days.Day7(day7) where

import Solver

import Control.Applicative ((<|>))
import Control.Arrow
import Data.Char
import Data.Maybe
import Data.List (foldl')
import Data.Map.Lazy (Map, empty, toList, alter)
import Text.ParserCombinators.ReadP

data Token = Tfile Int | Tcd String | TcdDot deriving (Show)

tokenize :: String -> [Token]
tokenize = lines >>> mapMaybe (readP_to_S (cd <|> file) >>> reverse >>> listToMaybe >>> fmap fst)
    where cd = string "$ cd " *> (TcdDot <$ string "..") <++ (Tcd <$> many1 (satisfy isPrint))
          file = fmap Tfile $ (read) <$> (many1 (satisfy isDigit) <* char ' ') <* (many1 (satisfy isPrint))

makeMap ::  Map [String] Int -> [Token] -> Map [String] Int
makeMap h = foldl' step ([], h) >>> snd where 
        step :: ([String], Map [String] Int) -> Token -> ([String], Map [String] Int)
        step (path, sizes) (Tcd s) = (s:path, sizes)
        step (_:ps, sizes) (TcdDot) = (ps, sizes)
        step (path, sizes) (Tfile size) = (path, updateMap path size sizes)
        step ([], _) TcdDot = error "cant move past root directory"

        updateMap :: [String] -> Int -> Map [String] Int -> Map [String] Int
        updateMap [] _ sizes = sizes
        updateMap x@(_:xs) size sizes = updateMap xs size (alter (\s -> case s of 
            (Just i) -> Just (size+i)
            Nothing -> Just size) x sizes)

day7 :: Solver
day7 = mkSolver 7 "No Space Left On Device" $ tokenize >>> makeMap empty >>> toList >>> fmap snd >>> ((filter (<= 100000) >>> sum) &&& ((\f -> filter (>  maximum f-40000000) f) >>> minimum))