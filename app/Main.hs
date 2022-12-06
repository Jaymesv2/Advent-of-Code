module Main (main) where

import Lib

main :: IO ()
main = solutionsTable >>= putStrLn