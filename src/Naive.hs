module Naive where

import Numeric.LinearAlgebra
import Prelude hiding ((<>))

napraviMatUKol :: [Double] -> [Double] -> [Double] -> Matrix Double
napraviMatUKol t1 t2 t3 =  col t1 ||| col t2 ||| col t3

nadjiLambda :: Matrix Double -> Matrix Double -> Double
nadjiLambda m1 m2 = det m1 / det m2

pomnoziLambaSa :: Matrix Double -> Matrix Double -> [Double] -> [Double]
pomnoziLambaSa m1 m2 = map (nadjiLambda m1 m2 *)

{- kolona(i) = lamda(i) * t(i) = det / det(i) * t(i)-}
naivnoKanonskoProjPresl :: [Double] -> [Double] -> [Double] -> [Double] -> Matrix Double
naivnoKanonskoProjPresl t1 t2 t3 t4 = napraviMatUKol lA lB lC
    where   lA  = pomnoziLambaSa (napraviMatUKol t4 t2 t3) det t1
            lB  = pomnoziLambaSa (napraviMatUKol t1 t4 t3) det t2
            lC  = pomnoziLambaSa (napraviMatUKol t1 t2 t4) det t3
            det = napraviMatUKol t1 t2 t3

naivnoProjPresl :: [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> [Double] -> Matrix Double
naivnoProjPresl t1 t2 t3 t4 t5 t6 t7 t8 = p2 <> p1
    where   p1 = inv (naivnoKanonskoProjPresl t1 t2 t3 t4)
            p2 = naivnoKanonskoProjPresl t5 t6 t7 t8

{- primer:
    naivnoProjPresl [-3,-1,1] [3,-1,1] [1,1,1] [-1,1,1] [-2,-1,1] [2,-1,1] [2,1,1] [-2,1,1] 
-}
