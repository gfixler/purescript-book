module Main where

import Prelude

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)


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


-- 4.1.2. (Medium) Write a recursive function which counts the number of even
-- integers in an array. Hint: the function unsafePartial head (where head is
-- also imported from Data.Array.Partial) can be used to find the first element
-- in a non-empty array.

countEvens :: Array Int -> Int
countEvens arr =
    if null arr
        then 0
        else (if isEven (unsafePartial head arr) then 1 else 0) + countEvens (unsafePartial tail arr)
