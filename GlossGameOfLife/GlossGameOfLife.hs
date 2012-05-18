module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import Graphics.Gloss.Data.Display (Display (InWindow))
import Graphics.Gloss.Data.Color (Color, greyN)
import Graphics.Gloss.Interface.Simulate (simulate)
import Parser (parseBoardFromFile)
import Evolution (evolve)
import Drawing (drawModel, cellSize)


main =
    do  fps_ : filename : _ <- getArgs

        initial <- parseBoardFromFile filename
        let fps = read fps_ :: Int

        let (l,c) = (length initial, length $ head initial)
            scale n = floor (fromIntegral n * cellSize)
            windowSize = (scale c, scale l)
            display = InWindow title windowSize windowPosition

        simulate display bg fps initial drawModel (\_ _ b -> evolve b)


title :: String
title = "Conway's Game of Life"

windowPosition :: (Int, Int)
windowPosition = (100,0)

bg :: Color
bg = greyN 0.95
