{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib where

import Numeric.LinearAlgebra
import Naive (naivnoProjPresl)
import Dlt 
import DltNorm 

someFunc :: IO ()
someFunc = print ()


skalMatDaPrvi1 :: Matrix Double -> Matrix Double
skalMatDaPrvi1 m = reshape 3 mp
    where   mp = podeliVekt delilac (flatten m)
            delilac = atIndex m (0,0)



a1 = skalMatDaPrvi1 $ naivnoProjPresl y1 y2 y3 y4 y1p y2p y3p y4p
a2 = skalMatDaPrvi1 $ dlt [y1, y2, y3, y4] [y1p, y2p, y3p, y4p]
a3 = skalMatDaPrvi1 $ dltMod [y1, y2, y3, y4] [y1p, y2p, y3p, y4p]
a4 = skalMatDaPrvi1 $ dlt [y1, y2, y3, y4, y5] [y1p, y2p, y3p, y4p, y5p]
a5 = skalMatDaPrvi1 $ dltMod [y1, y2, y3, y4, y5] [y1p, y2p, y3p, y4p, y5p]
a6 = skalMatDaPrvi1 $ dltMod [yn1, yn2, yn3, yn4, yn5] [yn1p, yn2p, yn3p, yn4p, yn5p]



{- test primer -}
y1 :: [Double]
y1=[2,1,1]
y2 :: [Double]
y2=[1,2,1]
y3 :: [Double]
y3=[3,4,1]
y4 :: [Double]
y4=[-1,-3,1]
y5 :: [Double]
y5=[-2,5,1]

y1p :: [Double]
y1p=[0,1,1]
y2p :: [Double]
y2p=[5,0,1]
y3p :: [Double]
y3p=[2,-5,1]
y4p :: [Double]
y4p=[-1,-1,1]
y5p :: [Double]
y5p=[4,1,2]

yn1 :: [Double]
yn1=[0,-3,1]
yn2 :: [Double]
yn2=[0,-1,1]
yn3 :: [Double]
yn3=[4,-1,1]
yn4 :: [Double]
yn4=[-7,-4,1]
yn5 :: [Double]
yn5=[0,5,1]

yn1p :: [Double]
yn1p=[3,-1,1]
yn2p :: [Double]
yn2p=[4,4,1]
yn3p :: [Double]
yn3p=[9,1,1]
yn4p :: [Double]
yn4p=[5,-2,1]
yn5p :: [Double]
yn5p=[7,2,2]



