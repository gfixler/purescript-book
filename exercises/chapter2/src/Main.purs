module Main where

import Prelude ((+), (*))
import Control.Monad.Eff.Console (logShow)
import Math (pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)


-- 1. (Easy) Use the pi constant, which is defined in the Math module, to write
-- a function circleArea which computes the area of a circle with a given
-- radius. Test your function using PSCi (Hint: don’t forget to import pi by
-- modifying the import Math statement).

circleArea :: Number -> Number
circleArea r = pi * r * r


main = logShow (diagonal 3.0 4.0)
