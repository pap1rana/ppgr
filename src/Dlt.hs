{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Dlt
where
{---------- DLT ------------}
import Numeric.LinearAlgebra.HMatrix

{- napraviMat2x9 [1,2,3] [4,5,6] -}
napraviMat2x9 :: (Num a, Element a) => [a] -> [a] -> Matrix a
napraviMat2x9 x [x1p,x2p,x3p] = fromLists [prviRed, drugiRed]
    where   prviRed  = [0,0,0] ++ [(-x3p)*i | i <- x] ++ [x2p*i | i <- x]
            drugiRed = [x3p*i | i <- x] ++ [0,0,0] ++ [(-x1p)*i | i <- x]

{- vertikalno nalepi -}
spojiMatDLT :: (Num a, Element a) => [Matrix a] -> Matrix a
spojiMatDLT [] = (0><9) []
spojiMatDLT l  = head l === spojiMatDLT (tail l)
    

{- svd m= Numeric.LinearAlgebra.HMatrix.svd $ (Numeric.LinearAlgebra.HMatrix.fromList (vector Data.Matrix.toList m)) -}


dlt :: (Num a) => [[a]] -> [[a]] -> Int
dlt l lp = length l
