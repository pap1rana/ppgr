{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Dlt
where
{---------- DLT ------------}
import Numeric.LinearAlgebra

{- napraviMat2x9 [1,2,3] [4,5,6] -}
napraviMat2x9 :: (Num a, Element a) => [a] -> [a] -> Matrix a
napraviMat2x9 x [x1p,x2p,x3p] = fromLists [prviRed, drugiRed]
    where   prviRed  = [0,0,0] ++ [(-x3p)*i | i <- x] ++ [x2p*i | i <- x]
            drugiRed = [x3p*i | i <- x] ++ [0,0,0] ++ [(-x1p)*i | i <- x]

{- vraca listu matrica -}
generisiSve2x9 :: (Num a, Element a) => [[a]] -> [[a]] -> [Matrix a]
generisiSve2x9 [] [] = []
generisiSve2x9 l  lp = napraviMat2x9 (head l) (head lp) : generisiSve2x9 (tail l) (tail lp)

{- vertikalno nalepi -}
spojiMatDLT :: (Num a, Element a) => [Matrix a] -> Matrix a
spojiMatDLT [] = (0><9) []
spojiMatDLT l  = head l === spojiMatDLT (tail l)

{- poslednji kolona vT je nasa matrica-}
matProjPreslDLT :: (Field a) => Matrix a -> Matrix a
matProjPreslDLT m = reshape 1 (flatten $ dropColumns (cols vT-1) vT)
    where (u,d,vT) = svd m


{- dlt [a,b,c,d..] [ap,bp,cp,dp..]-}
dlt :: Field a => [[a]] -> [[a]] -> Matrix a
dlt l lp = matProjPreslDLT $ spojiMatDLT (generisiSve2x9 l lp)

formatDLT :: (Element t, Ord t, Fractional t) => Matrix t -> Matrix t
formatDLT m = reshape 3 (fromList $ map zaokruzi (toList $ flatten m))
    where zaokruzi = \x -> if x<0.001 && x>(-0.001) then 0 else x

{- primer sa predavanja i provera da li je x'=p*x tj x' vekt px = 0  
        x   = [1,2,3]
        xp  = [4,5,6]
        xxp = napraviMat2x9 x xp
        mp  = matProjPreslDLT xxp
        xxp Numeric.LinearAlgebra.<> mp

    primer iz naivnog -> 1x9 mat:
        mpdlt = dlt [[-3,-1,1],[3,-1,1],[1,1,1],[-1,1,1]] [[-2,-1,1],[2,-1,1],[2,1,1],[-2,1,1]]
    za 3x3:
        formatDLT mpdlt
    
    e  = col [1,2,3]
    ep = mpdlt <> e
    
 -}