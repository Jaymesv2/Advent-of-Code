module Lib
    ( splitOn
    , groupsOf
    ) where

splitOn :: (a->Bool) -> [a] -> [[a]]
splitOn on s = uncurry (:) $ fmap f $ break on s 
    where f x = if null x then [] else splitOn on $ tail x

groupsOf :: Int -> [a] -> [[a]]
groupsOf size lst = case splitAt size lst of
    (x, []) -> [x]
    (x, y) -> x:groupsOf size y