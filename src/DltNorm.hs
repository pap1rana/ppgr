{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DltNorm where
import Numeric.LinearAlgebra.HMatrix


nadjiTeziste :: Fractional a => [[a]] -> [a]
nadjiTeziste tacke = [prosekX, prosekY, prosekZ]
    where   prosekX = prosek $ map head tacke
            prosekY = prosek $ map (head . tail) tacke
            prosekZ = prosek $ map last tacke
            prosek l = sum l / fromIntegral (length l)


matTranUKoordPoc :: [Double] -> Matrix Double
matTranUKoordPoc [x,y,_] = col [1,0,0] ||| col [0,1,0] ||| col [-x,-y,0]



