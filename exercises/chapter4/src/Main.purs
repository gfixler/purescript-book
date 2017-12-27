module Main where

import Prelude


-- 4.1.1. (Easy) Write a recursive function which returns true if and only if
-- its input is an even integer.

-- exceeds call stack (for me after 17950)
isEven :: Int -> Boolean
isEven 0 = true
isEven n = not $ isEven (n - 1)

-- call stack safe; tail-call optimized
isEven' :: Int -> Boolean
isEven' = go true
    where
    go :: Boolean -> Int -> Boolean
    go true 0 = true
    go false 0 = false
    go b n = go (not b) (n - 1)
