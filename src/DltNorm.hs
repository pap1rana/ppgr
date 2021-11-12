{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module DltNorm where
import Numeric.LinearAlgebra
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
{- matSkalRastPros koliko l = col [lambda,0,0] ||| col [0,lambda,0] ||| col [0,0,1] -}
matSkalRastPros koliko l = diag $ fromList [lambda,lambda,1]
    where   lambda = koliko / rastProsecno l


matNorm :: [[Double]] -> Matrix Double
matNorm lt = s <> g
    where   g = matSkalRastPros (sqrt 2) transliraneTacke
            s = matTranDo00 (nadjiTeziste lt)
            transliraneTacke = map (((toList . flatten) . (s<>)) . col) lt


tranTacke :: Matrix Double -> [[Double]] -> [[Double]]
tranTacke m = map (((toList . flatten) . (m<>)) . col)

normTacke :: [[Double]] -> [[Double]]
normTacke lt = tranTacke (matNorm lt) lt


izjednaciMatPoPoz :: Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double
izjednaciMatPoPoz v k m1 m2 = reshape 3 m2p
    where   m2p = pomnoziVekt mnozilac $ podeliVekt delilac (flatten m2)
            delilac = atIndex m2 (v,k)
            mnozilac = atIndex m1 (v,k)

pomnoziVekt :: Double -> Vector Double -> Vector Double
pomnoziVekt x v = fromList $ map (*x) (toList v)

podeliVekt :: Double -> Vector Double -> Vector Double
podeliVekt x v = fromList $ map (/x) (toList v)

{- 
    xs      = [[1,1,1],[5,2,1],[6,4,1],[-1,7,1]]
    xps     = [[0,0,1],[10,0,1],[10,5,1],[0,5,1]]
    mdlt    = dlt xs xps
    mdltn   = dlt (normTacke xs) (normTacke xps)
    mdltn2  = izjednaciMatPoPoz 0 0 mdlt mdltn

 -}





