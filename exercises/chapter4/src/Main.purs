module Main where

import Prelude

import Data.Array ((..), (:), filter, length, mapMaybe, null)
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl)
import Partial.Unsafe (unsafePartial)
import Data.Foldable (product)
import Control.MonadZero (guard)

import FileOperations (allFiles)
import Data.Path (Path, isDirectory, ls, size)


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


-- 4.2.1. (Easy) Use the map or <$> function to write a function which calculates
-- the squares of an array of numbers.

squareAll :: Array Number -> Array Number
squareAll = map (\x -> x * x)


-- 4.2.2 (Easy) Use the filter function to write a function which removes the
-- negative numbers from an array of numbers.

removeNegs :: Array Number -> Array Number
removeNegs = filter (\x -> x >= 0.0)


-- 4.2.3 (Medium) Define an infix synonym <$?> for filter. Rewrite your answer
-- to the previous question to use your new operator. Experiment with the
-- precedence level and associativity of your operator in PSCi.

-- settled on 5 as lowest that allows: f $? arr1 <> g $? arr2
infixl 5 filter as $?

removeNegs' :: Array Number -> Array Number
removeNegs' xs = (<=) 0.0 $? xs


-- 4.3.1 (Easy) Use the factors function to define a function isPrime which
-- tests if its integer argument is prime or not.

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ do
    i <- 1 .. n
    j <- i .. n
    [[i, j]]

isPrime :: Int -> Boolean
isPrime = (==) 1 <<< length <<< factors


-- 4.3.2 (Medium) Write a function which uses do notation to find the cartesian
-- product of two arrays, i.e. the set of all pairs of elements a, b, where a
-- is an element of the first array, and b is an element of the second.

cartProd :: forall a. Array a -> Array a -> Array (Array a)
cartProd xs ys = do
    x <- xs
    y <- ys
    pure [x, y]


-- 4.3.3 (Medium) A Pythagorean triple is an array of numbers [a, b, c] such
-- that a² + b² = c². Use the guard function in an array comprehension to write
-- a function triples which takes a number n and calculates all Pythagorean
-- triples whose components are less than n. Your function should have type Int
-- -> Array (Array Int).

triples :: Int -> Array (Array Int)
triples n = do
    a <- 1 .. n
    b <- a .. n
    c <- b .. n
    guard $ a * a + b * b == c * c
    pure [a, b, c]


-- 4.3.4 (Difficult) Write a function factorizations which produces all
-- factorizations of an integer n, i.e. arrays of integers whose product is n.
-- Hint: for an integer greater than 1, break the problem down into two
-- subproblems: finding the first factor, and finding the remaining factors.

-- factorizations :: Int -> Array (Array Int)
-- factorizations 1 = pure [1]
-- factorizations n = do
--     TODO

-- 4.4.1 (Easy) Use foldl to test whether an array of boolean values are all
-- true.

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- 4.4.2 (Medium) Characterize those arrays xs for which the function
-- foldl (==) false xs returns true.

-- Arrays with an odd number of false elements, and only these, return true.

-- 4.4.3 (Medium) Rewrite the following function in tail recursive form using
-- an accumulator parameter:

-- import Prelude
-- import Data.Array.Partial (head, tail)
--
-- count :: forall a. (a -> Boolean) -> Array a -> Int
-- count _ [] = 0
-- count p xs = if p (unsafePartial head xs)
--                then count p (unsafePartial tail xs) + 1
--                else count p (unsafePartial tail xs)

count :: forall a. (a -> Boolean) -> Array a -> Int
count p = go 0
    where
    go c [] = c
    go c xs = if p (unsafePartial head xs)
                  then go (c+1) (unsafePartial tail xs)
                  else go c (unsafePartial tail xs)

-- 4.4.3 (Medium) Write reverse in terms of foldl.

reverse' :: forall a. Array a -> Array a
reverse' = foldl (\xs x -> x : xs) []

-- 4.5.1 (Easy) Write a function onlyFiles which returns all files
-- (not directories) in all subdirectories of a directory.

onlyFiles :: Path -> Array Path
onlyFiles file =
    if (isDirectory file)
       then do
           child <- ls file
           onlyFiles child
       else file : do
           child <- ls file
           onlyFiles child

onlyFiles' :: Path -> Array Path
onlyFiles' file = if (isDirectory file) then files else (file : files)
    where files = do
            child <- ls file
            onlyFiles' child

onlyFiles'' :: Path -> Array Path
onlyFiles'' = filter (not <<< isDirectory) <<< allFiles

-- 4.5.2 (Medium) Write a fold to determine the largest and smallest files in
-- the filesystem.

smallestAndLargestFiles :: Path -> Array Int
smallestAndLargestFiles xs = foldl
                             (unsafePartial (\[l, h] x -> [min l x, max h x]))
                             [999999999, -1]
                             ss
    where ss = mapMaybe size (onlyFiles xs)

