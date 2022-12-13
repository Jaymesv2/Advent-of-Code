{-# LANGUAGE TupleSections #-}
module Days.Day12(day12) where

import Solver

import Data.Graph
import Data.Char

import Control.Arrow
import Debug.Trace
import Data.Maybe

import Algorithm.Search

{-
import Data.GraphViz
import Data.GraphViz.Commands
import System.IO.Unsafe
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as P

runMe :: IO String
runMe = do
    input <- readFile "inputs/12.txt"
    let (graph, vert, key) = makeGraph $ parseInput input
        unwrap s = case s of
            Just n -> n
            Nothing -> error "failed to unwrap"
        x :: Vertex -> G.Context Char Int
        x v = ([], v, l, ((1,) . (unwrap . key)) <$> n) where (l, _, n) = vert v
        fglGraph :: P.Gr Char Int
        fglGraph = G.buildGr (x <$> vertices graph)
        gv = (setDirectedness graphToDot params) $  fglGraph
        params = quickParams
    runGraphvizCanvas' gv Xlib
    pure "hi"
-}

solve :: (GraphG -> [Key]) -> GraphG -> Int
solve getPos g@(_,vert,key) = minimum (mapMaybe getDist $ getPos g)
    where 
          getDist start = length . snd <$> dijkstra getNeighbors (\_ _ -> 1) ((==) end) start
          end = head $ allXPos 'E' g
          getNeighbors s = let (_,_,n) = unwrap $ vert <$> key s in n
          unwrap s = case s of
            Just n -> n
            Nothing -> error "failed to unwrap"

solve :: (GraphG -> [Key]) -> GraphG -> Int
solve getPos g@(_,vert,key) = minimum (mapMaybe getDist $ getPos g)
    where 
          getDist start = length . snd <$> dijkstra getNeighbors (\_ _ -> 1) ((==) end) start
          end = head $ allXPos 'E' g
          getNeighbors s = let (_,_,n) = unwrap $ vert <$> key s in n
          unwrap s = case s of
            Just n -> n
            Nothing -> error "failed to unwrap"

-- a list of rows
type Matrix a = [[a]]
type Key = (Int, Int)
type GraphG = (Graph, Vertex -> (Char, Key, [Key]), Key -> Maybe Vertex)

day12 :: Solver
day12 = mkSolver 12 "Hill Climbing Algorithm" $ lines >>> makeGraph >>> (solve (allXPos 'S') &&& solve (allXPos 'a'))

allXPos :: Char -> GraphG -> [Key]
allXPos x (g,vert, _) = mapMaybe filterPos (vertices g)
    where filterPos v = if c == x then Just k else Nothing where (c,k,_) = vert v 

makeGraph :: Matrix Char -> GraphG
makeGraph matrix = graphFromEdges [go (m,n) | m <- [0..lm-1], n <- [0..ln-1]]
    where
        lm = length matrix
        ln = length (head matrix)
        mAt :: Key -> Int
        mAt (m,n) = case (matrix !! m) !! n of 
                'S' -> 0 -- S
                'E' -> 26  -- E
                c   -> ord c - 97
        go :: Key -> (Char, Key, [Key])
        go (m,n) = ((matrix !! m) !! n, (m,n), neighbors)
            where neighbors = filter (\(x, y) -> (x >= 0) && (x < lm) && (y >= 0) && (y < ln) && (mAt (m,n) - mAt (x,y) >= -1))
                    [(m+1, n), (m-1, n), (m, n+1), (m, n-1)]