{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
    {- ( someFunc
    ) -} where

import Dom1
import Data.Matrix

someFunc :: IO ()
someFunc = print (naivnoProjPresl [-3,-1,1] [3,-1,1] [1,1,1] [-1,1,1] [-2,-1,1] [2,-1,1] [2,1,1] [-2,1,1])

napraviMatUKol :: (Num a) => [a] -> [a] -> [a] -> Matrix a
napraviMatUKol t1 t2 t3 = transpose $ fromLists [t1, t2, t3]

nadjiLambda :: (Ord a, Fractional a) => Matrix a -> Matrix a -> a
nadjiLambda m1 m2 = detLaplace m1 / detLaplace m2

{- nadjiLamba i pomnozi sa -}
pomnoziLambda :: (Ord a, Fractional a) => Matrix a -> Matrix a -> [a] -> [a]
{- pomnoziLambda m1 m2 = map (nadjiLambda m1 m2 *)-}
pomnoziLambda m1 m2 = map (nadjiLambda m1 m2 *)

{- kolona(i) = lamda(i) * t(i) = det / det(i) -}
naivnoKanonskoProjPresl:: (Ord a, Fractional a) => [a] -> [a] -> [a] -> [a] -> Matrix a
naivnoKanonskoProjPresl t1 t2 t3 t4 = napraviMatUKol lA lB lC
    where   lA  = pomnoziLambda (napraviMatUKol t4 t2 t3) det t1
            lB  = pomnoziLambda (napraviMatUKol t1 t4 t3) det t2
            lC  = pomnoziLambda (napraviMatUKol t1 t2 t4) det t3
            det = napraviMatUKol t1 t2 t3

naivnoProjPresl :: (Fractional a, Ord a) => [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] ->  Matrix a
naivnoProjPresl t1 t2 t3 t4 t5 t6 t7 t8 =
    case inverz of
        Left err -> zero 3 3
        Right p1 -> multStd p2 p1
    where   inverz = inverse (naivnoKanonskoProjPresl t1 t2 t3 t4)
            p2 = naivnoKanonskoProjPresl t5 t6 t7 t8

{- primer zadatak:
    naivnoProjPresl [-3,-1,1] [3,-1,1] [1,1,1] [-1,1,1] [-2,-1,1] [2,-1,1] [2,1,1] [-2,1,1] -}


{- DLT -}


napraviMat2x9 :: (Num a) => [a] -> [a] -> Matrix a
napraviMat2x9 x [x1p,x2p,x3p] = fromLists [prviRed, drugiRed]
    where   prviRed  = [0,0,0] ++ [(-x3p)*i | i <- x] ++ [x2p*i | i <- x]
            drugiRed = [x3p*i | i <- x] ++ [0,0,0] ++ [(-x1p)*i | i <- x]
