module Days.Day7(day7) where

import Solver
--import Util

import Debug.Trace

import Data.List(foldl', intercalate)
import Data.Char
import Data.Maybe

import Control.Applicative ((<|>))
import Control.Monad.Trans.Writer.Lazy
import Control.Arrow

import Text.ParserCombinators.ReadP

data Token = Tfile (Int)
           | Tcd
           | TcdDot
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize = mapMaybe (fmap fst . listToMaybe . reverse . readP_to_S (cd <|> file)) . lines
    where cd = string "$ cd " *> (TcdDot <$ string "..") <++ (Tcd <$ many1 (satisfy isPrint))
          file = fmap Tfile $ (read) <$> (many1 (satisfy isDigit) <* char ' ') <* (many1 (satisfy isPrint))

day7 :: Solver
day7 = mkSolver 7 "No Space Left On Device" $ \inp -> 
    let tokens = tokenize inp
        y = finishEval . foldl' evalToken ([],[]) $ tokens
    in ("incomplete", "incomplete")

data File a = Dir [File a] 
          | File a
          deriving (Eq, Show)

type State = ([File Int], [File Int])

evalToken :: State -> Token -> State
evalToken (x,    fs) Tcd           = (Dir fs:x, [])
evalToken ((Dir ds):xs, fs) TcdDot = (xs, (Dir fs):ds)
evalToken (_,    s) TcdDot         = error ""
evalToken (x,    ds) (Tfile s)     = (x, File s:ds)

finishEval :: State -> File Int
finishEval ((Dir ds):xs, fs) = finishEval (xs, (Dir fs):ds)
finishEval ([], fs) = Dir fs

{-
To begin, find all of the directories with a total size of at most 100000, 
then calculate the sum of their total sizes.
(As in this example, this process can count files more than once!)
-}


