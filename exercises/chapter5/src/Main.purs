module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Picture

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

-- 5.3.1 (Easy) Construct a value of type Shape which represents a circle
-- centered at the origin with radius 10.0.

aCircle :: Shape
aCircle = Circle (Point { x: 0.0, y: 0.0 }) 10.0

-- 5.3.2 (Medium) Write a function from Shapes to Shapes, which scales its
-- argument by a factor of 2.0, center the origin.

scalePoint :: Number -> Point -> Point
scalePoint n (Point { x, y }) = Point { x: x * n, y: y * n }

enlarge :: Shape -> Shape
enlarge (Circle p r) = Circle (scalePoint 2.0 p) (r * 2.0)
enlarge (Rectangle p w h) = Rectangle (scalePoint 2.0 p) (w * 2.0) (h * 2.0)
enlarge (Line p1 p2) = Line (scalePoint 2.0 p1) (scalePoint 2.0 p2)
enlarge (Text p s) = Text (scalePoint 2.0 p) s

-- 5.3.3 (Medium) Write a function which extracts the text from a Shape. It
-- should return Maybe String, and use the Nothing constructor if the input is
-- not constructed using Text.

getShapeText :: Shape -> Maybe String
getShapeText (Text _ s) = Just s
getShapeText _ = Nothing
