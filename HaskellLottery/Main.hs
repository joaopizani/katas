module Main where

import System.IO (getLine, readFile)
import Control.Monad (liftM, replicateM_)
import Data.List (sort)
import Data.IntSet as I
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State


data Combination = Combination {
    combinationLucky :: I.IntSet,
    combinationUnlucky :: I.IntSet
} deriving Eq

instance Show Combination where
    show (Combination l u) = "[" ++ showNumbers l ++ "] " ++ showNumbers u
        where showNumbers = unwords . fmap show . I.toList

instance Ord Combination where
    compare (Combination l1 _) (Combination l2 _) = (flip compare) (I.size l1) (I.size l2)


readMatrix :: FilePath -> IO [[Int]]
readMatrix fileName = liftM (fmap parseLine . lines) $ readFile fileName
    where parseLine = fmap read . words 

makeCombination :: [Int] -> Combination
makeCombination xs = Combination I.empty (I.fromList xs)

makeCombinations :: [[Int]] -> [Combination]
makeCombinations = liftM makeCombination

readCombinations :: FilePath -> IO [Combination]
readCombinations = liftM makeCombinations . readMatrix


matchWithCombination :: Int -> Combination -> Combination
probe `matchWithCombination` (Combination l u) = Combination l' u'
    where   l' = l `I.union` (u `I.intersection` (I.singleton probe))
            u' = u `I.difference` l'

matchWithCombinations :: Int -> [Combination] -> [Combination]
matchWithCombinations probe = liftM (matchWithCombination probe)


askForNewNumber :: IO Int
askForNewNumber = getLine >>= return . read

includeNewNumber :: StateT [Combination] IO ()
includeNewNumber = 
    do  probe <- lift askForNewNumber
        currentCombs <- get
        put $ probe `matchWithCombinations` currentCombs

includeAndPrintTop5 :: StateT [Combination] IO ()
includeAndPrintTop5 =
    do  cs <- includeNewNumber >> get
        lift $ putStrLn $ unlines $ fmap show $ take 5 $ sort $ cs


main = 
    do  combs <- readCombinations "combinations.txt"
        execStateT (replicateM_ 6 includeAndPrintTop5) combs
