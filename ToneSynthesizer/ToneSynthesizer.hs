module Main where

import qualified Data.ByteString.Lazy as B
import Text.Parsec.ByteString (parseFromFile)
import Data.Either (either)
import System.Environment (getArgs)
import Parameters (concrete)
import Melody (score)
import Signal (slice, signal)


main =
    do  args <- getArgs
        let file : bpmStr : _ = args
            bpm = (read bpmStr) :: Int

        score <- parseFromFile score file 
        let abstractMusic = either (error . show) id score
            concreteMusic = map (concrete bpm) abstractMusic

        B.putStr $ produceSampleStream concreteMusic


-- | Given the sequence of concrete musical sounds to play
-- (Frequency, Duration), produce the corresponding sample stream
produceSampleStream :: [(Float,Float)] -> B.ByteString
produceSampleStream music = B.concat [slice t (signal f)  | (f,t) <- music]
