module Signal ( signal,
                slice ) where

import qualified Data.ByteString.Lazy as B
import Parameters
import Functions


-- | Get a slice of an infinite signal with n seconds
slice :: Float -> B.ByteString -> B.ByteString
slice time = B.take (truncate (fromIntegral rate * time))


-- | Makes a continuous (infinite) signal for a given frequency
signal freq = cycleAndPack (singlePeriod freq)
    where
        singlePeriod 0 = silence
        singlePeriod f = period (truncate $ fromIntegral rate / f) Square


-- | Converts a [Int] to a Lazy ByteString and then cycles it
cycleAndPack :: [Int] -> B.ByteString
cycleAndPack = B.cycle . B.pack . (map fromIntegral) 


-- | Synthesizes one period
period :: Int -> Waveform -> [Int]
period n Square = scale codomain $ map square (normalizeDom n)
period n Sine = scale codomain $ map sin (zeroTo2Pi n)


-- | Synthesizes one small silence, one sample :)
silence :: [Int]
silence = [0]
