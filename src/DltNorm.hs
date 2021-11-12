{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DltNorm where
import Numeric.LinearAlgebra.HMatrix
import Prelude hiding ((<>))

prosek :: [Double] -> Double
prosek l = sum l / fromIntegral (length l)

{- [t1,t2..] -> c -}
nadjiTeziste :: [[Double]] -> [Double]
nadjiTeziste tacke = [prosekX, prosekY, prosekZ]
    where   prosekX = prosek $ map head tacke
            prosekY = prosek $ map (head . tail) tacke
            prosekZ = prosek $ map last tacke

matTranDo00 :: [Double] -> Matrix Double
matTranDo00 [x,y,_] = col [1,0,0] ||| col [0,1,0] ||| col [-x,-y,1]


{- tacka -> rast -}
rastOd00 :: [Double] -> Double
rastOd00 [x,y,_] = sqrt $ x^2 + y^2

rastProsecno :: [[Double]] -> Double
rastProsecno l = prosek $ map rastOd00 l

matSkalRastPros :: Double -> [[Double]] -> Matrix Double
matSkalRastPros koliko l = col [lambda,0,0] ||| col [0,lambda,0] ||| col [0,0,1]
    where   lambda = koliko / rastProsecno l


matNorm :: [[Double]] -> Matrix Double
matNorm lt = s <> g
    where   g = matSkalRastPros (sqrt 2) transliraneTacke
            s = matTranDo00 (nadjiTeziste lt)
            transliraneTacke = map (((toList . flatten) . (s<>)) . col) lt


