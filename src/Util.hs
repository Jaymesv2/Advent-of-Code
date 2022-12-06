module Util 
    ( splitOn
    , groupsOf
    , windows
    , windowsN
    , enumerate
    ) where

splitOn :: (a->Bool) -> [a] -> [[a]]
splitOn on s = uncurry (:) $ fmap f $ break on s 
    where f x = if null x then [] else splitOn on $ tail x

groupsOf :: Int -> [a] -> [[a]]
groupsOf size lst = case splitAt size lst of
    (x, []) -> [x]
    (x, y) -> x:groupsOf size y

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows n xs = let window = take n xs
    in if length window < n then [] else window:windows n (tail xs)

windowsN :: Int -> [a] -> [(Int, [a])]
windowsN n = enumerate . windows n

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

union :: Eq a => [a] -> [a] -> [a]
union xs ys = [x | x <- xs, y <- ys, x == y]