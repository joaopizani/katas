module Main where

import Data.List (transpose, lookup)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)

main = interact (\input -> parse input ++ "\n")

parse i = map (getDigit . transpose . tail) $ chunksOf 4 $ dummy : columns i
    where getDigit = fromJust . (flip lookup) dTable
          columns  = transpose . lines
          dummy    = "   "

dTable = [
           ([" _ ",
             "| |",
             "|_|"], '0'),
           (["   ",
             "  |",
             "  |"], '1'),
           ([" _ ",
             " _|",
             "|_ "], '2'),
           ([" _ ",
             " _|",
             " _|"], '3'),
           (["   ",
             "|_|",
             "  |"], '4'),
           ([" _ ",
             "|_ ",
             " _|"], '5'),
           ([" _ ",
             "|_ ",
             "|_|"], '6'),
           ([" _ ",
             "  |",
             "  |"], '7'),
           ([" _ ",
             "|_|",
             "|_|"], '8'),
           ([" _ ",
             "|_|",
             " _|"], '9') ]
