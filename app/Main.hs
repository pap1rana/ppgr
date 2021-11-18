{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where

import Lib
import System.Environment
import Numeric.LinearAlgebra(loadMatrix, saveMatrix, toLists)
import Naive (naivnoProjPresl)

{- main :: IO ()
main = someFunc
 -}

main :: IO ()
main = do
    m <- loadMatrix "tacke.txt"
    let o = take 4 $ toLists m
    let s = drop 4 $ toLists m
    let matricaPreslikavanja = naivnoProjPresl (o!!0) (o!!1) (o!!2) (o!!3) (s!!0) (s!!1) (s!!2) (s!!3) 
    saveMatrix  "matPreslikavanja.txt" "%.5f" matricaPreslikavanja

    