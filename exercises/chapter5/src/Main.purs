module Main where

import Prelude


-- 5.1.1 (Easy) Write the factorial function using pattern matching. Hint.
-- Consider the two cases zero and non-zero inputs.

fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n - 1)

-- 5.1.2 (Medium) Look up Pascal’s Rule for computing binomial coefficients.
-- Use it to write a function which computes binomial coefficients using
-- pattern matching.

choose :: Int -> Int -> Int
choose _ 0 = 1
choose n k | n < k  = 0
           | otherwise = choose (n-1) (k-1) + choose (n-1) k

-- 5.2.1 (Easy) Write a function sameCity which uses record patterns to test
-- whether two Person records belong to the same city.

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c } } { address: { city: c' } } = c == c'
