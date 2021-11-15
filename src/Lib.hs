{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib where

import Naive (naivnoProjPresl)
import Dlt ()
import DltNorm ()

someFunc :: IO ()
someFunc = print (naivnoProjPresl [-3,-1,1] [3,-1,1] [1,1,1] [-1,1,1] [-2,-1,1] [2,-1,1] [2,1,1] [-2,1,1])






