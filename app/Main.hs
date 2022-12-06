{-# LANGUAGE RankNTypes, GADTs #-}
module Main (main) where

import Lib

main :: IO ()
main = solutions >>= sequence_ . fmap print