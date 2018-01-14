module Main where

import Prelude


-- 5.1.1 (Easy) Write the factorial function using pattern matching. Hint.
-- Consider the two cases zero and non-zero inputs.

fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n - 1)
