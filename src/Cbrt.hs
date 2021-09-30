module Cbrt where

type Hontaikakaku = Double
type Zeikomikakaku = Double
zeikomi :: Hontaikakaku ->  Zeikomikakaku
zeikomi x = 1.1 * x

cube :: Double -> Double 
cube x = x * x * x
