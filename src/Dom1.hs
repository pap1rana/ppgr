{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Dom1 where

data Tacka = MkTacka    { x1 :: Double
                        , x2 :: Double
                        , x3 :: Double
                        }
t :: Double -> Double -> Tacka
t a b = MkTacka a b 1

instance Show Tacka where
    show (MkTacka x1 x2 x3) = "(" ++ show x1 ++ " : " ++ show x2 ++ " : " ++ show x3 ++ ")"

data Prava = MkPrava    { a :: Double
                        , b :: Double
                        , c :: Double
                        }
instance Show Prava where
    show (MkPrava a b c) = "[" ++ show a ++ " : " ++ show b ++ " : " ++ show c ++ "]"

nadjiPravu :: Tacka -> Tacka -> Prava
nadjiPravu p1 p2 = MkPrava a b c
    where a = (x2 p1 * x3 p2) - (x3 p1 * x2 p2)
          b = (x3 p1 * x1 p2) - (x1 p1 * x3 p2)
          c = (x1 p1 * x2 p2) - (x2 p1 * x1 p2)

nadjiTacku :: Prava -> Prava -> Tacka
nadjiTacku q r = MkTacka x1 x2 x3
    where x1 = (b q * c r) - (c q * b r)
          x2 = (c q * a r) - (a q * c r)
          x3 = (a q * b r) - (b q * a r)

naAfine :: Tacka -> Tacka
naAfine p = MkTacka (zaokruzi ((x1 p) / (x3 p))) (zaokruzi ((x2 p) / (x3 p))) ((x3 p) / (x3 p))

zaokruzi x = fromIntegral $ round x

usrednji :: Tacka -> Tacka -> Tacka -> Tacka
usrednji p1 p2 p3 = MkTacka (zaokruzi ((x1 p1 + x1 p2 + x1 p3)/3)) (zaokruzi ((x2 p1 + x2 p2 + x2 p3)/3)) (zaokruzi ((x3 p1 + x3 p2 + x3 p3)/3))
          
nevidljivo :: [Tacka] -> Tacka
nevidljivo [p1, p2, p3, p5, p6, p7, p8] = naAfine $ nadjiTacku (nadjiPravu p8 xb) (nadjiPravu p3 yb)
    where xb = usrednji xb1 xb2 xb3 
          yb = usrednji yb1 yb2 yb3
          xb1 = nadjiTacku (nadjiPravu p3 p7) (nadjiPravu p1 p5)
          xb2 = nadjiTacku (nadjiPravu p2 p6) (nadjiPravu p1 p5)
          xb3 = nadjiTacku (nadjiPravu p2 p6) (nadjiPravu p3 p7)
          yb1 = nadjiTacku (nadjiPravu p7 p8) (nadjiPravu p6 p5)
          yb2 = nadjiTacku (nadjiPravu p1 p2) (nadjiPravu p6 p5)
          yb3 = nadjiTacku (nadjiPravu p1 p2) (nadjiPravu p7 p8)

tackaUlst :: Tacka -> [Double]
tackaUlst x = [x1 x, x2 x, x3 x]

-- test 
-- nevidljivo [t 595 301, t 292 517, t 157 379, t 665 116, t 304 295, t 135 163, t 509 43]