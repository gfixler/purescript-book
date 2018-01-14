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

gary = { name: "Gary Fixler", address: { street: "Poppy Meadow St.", city: "Los Angeles" } }
mom  = { name: "Joyce Bernardini", address: { street: "15751 Triple Crown Ct.", city: "Fort Myers" } }
dad = { name: "Gary Fixler", address: { street: "1351 Sherwood Dr.", city: "Vineland" } }
karen = { name: "Karen Gaburo", address: { street: "1351 Sherwood Dr.", city: "Vineland" } }

-- 5.2.2 (Medium) What is the most general type of the sameCity function,
-- taking into account row polymorphism? What about the livesInLA function
-- defined above?

sameCity' :: forall c r1 r2 r3 r4. Eq c =>
             { address :: { city :: c | r1 } | r2 } ->
             { address :: { city :: c | r3 } | r4 } ->
             Boolean
sameCity' { address: { city: c } } { address: { city: c' } } = c == c'

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

livesInLA' :: forall r1 r2. { address :: { city :: String | r1 } | r2 } -> Boolean
livesInLA' { address: { city: "Los Angeles" } } = true
livesInLA' _ = false

-- 5.2.3 (Medium) Write a function fromSingleton which uses an array literal
-- pattern to extract the sole member of a singleton array. If the array is not
-- a singleton, your function should return a provided default value. Your
-- function should have type forall a. a -> Array a -> a

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _ = x
