module Functions ( square,
                   normalizeDom,
                   zeroTo2Pi,
                   scale,
                   Waveform(..) ) where

import Parameters


-- | Simple waveform datatype
data Waveform = Sine | Square
    deriving (Eq, Show, Read)


-- | Normalized half duty cycle square function.
-- Dom(square) = [0,1], Im(square) = {-1,1}
square :: Float -> Float
square x
    | x < 0.5   = -1.0
    | otherwise =  1.0


-- | Normalizes an interval [0,n] to [0,1]
normalizeDom :: Int -> [Float]
normalizeDom n = [(fromIntegral i) / n_ | i <- [0..n]]
    where n_ = fromIntegral n


-- | Transforms [0,n] into [0,2*pi]
zeroTo2Pi :: Int -> [Float]
zeroTo2Pi n = map (* (2*pi)) (normalizeDom n)


-- | Scale a normal interval (between -1 and 1) back to a given codomain
scale :: (Int, Int) -> [Float] -> [Int]
scale (low, high) normal = [round (low_ + d*x) | x <- zeroTo2]
    where zeroTo2 = map (+ 1) normal
          d = (high_ - low_) / 2
          low_ = fromIntegral low
          high_ = fromIntegral high
