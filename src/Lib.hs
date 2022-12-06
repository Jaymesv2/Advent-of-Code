module Lib
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
windowsN xs n = enumerate $ windows xs n

enumerate :: [a] -> [(Int, a)]
enumerate = enumerate' 0
    where enumerate' n (x:xs) = (n, x):enumerate' (n+1) xs
          enumerate' n [] = []

intersection :: Eq a => [a] -> [a] -> [a]
intersection xs ys = [x | x <- xs, y <- ys, x == y]

