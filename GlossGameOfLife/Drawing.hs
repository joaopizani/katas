module Drawing ( drawModel
               , cellSize ) where

import Data.List (replicate)
import Graphics.Gloss.Data.Picture (Picture, pictures, color, translate, rectangleSolid)
import Graphics.Gloss.Data.Color (dark, light, orange)
import Evolution


innerSize :: Float
innerSize = 10.0

cellSize :: Float
cellSize = innerSize + padSize
    where
        padSize = 0.15 * innerSize

paintActive, paintInactive :: Picture -> Picture
paintActive = color $ dark orange
paintInactive = color $ light orange

cell :: Picture
cell = rectangleSolid innerSize innerSize


data LayoutDirection = DirX | DirY
    deriving (Eq, Show, Ord)

layout :: LayoutDirection -> [Picture] -> [Picture]
layout dir pics = [move dir middle i p | (i,p) <- zip [0..(n-1)] pics]
    where
        n = length pics
        middle = fromIntegral $ floor (fromIntegral n / 2)

move :: LayoutDirection -> Int -> Int -> Picture -> Picture
move d pivot idx = 
    case d of
        DirX -> translate distance 0.0
        DirY -> translate 0.0 (negate distance) -- Y increases downwards
    where
        distance = fromIntegral (idx - pivot) * cellSize


applyFunctionList :: [a -> b] -> [a] -> [b]
applyFunctionList fs xs = [f x | (f,x) <- zip fs xs]

applyFunctionMatrix :: [[a -> b]] -> [[a]] -> [[b]]
applyFunctionMatrix fss xss = [applyFunctionList fs xs | (fs,xs) <- zip fss xss]


activate :: Board -> [[Picture]] -> [[Picture]]
activate board = applyFunctionMatrix (paintersFromBoard board)
    where
        paintersFromBoard = map (map painter)
        painter active = if active then paintActive else paintInactive


drawModel :: Board -> Picture
drawModel b = (layoutY . layoutX) $ activate b $ rawMatrix
    where
        (l,c) = (length b, length $ head b) 
        rawMatrix = replicate l $ replicate c $ cell
        layoutX = map (pictures . layout DirX)
        layoutY = pictures . layout DirY
