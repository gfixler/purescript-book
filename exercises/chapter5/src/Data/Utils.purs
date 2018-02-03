module Utils where

import Prelude
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)

strJoin :: String -> Array String -> String
strJoin _ [] = ""
strJoin _ [x] = x
strJoin s xs = h <> s <> strJoin s t
    where
    h = unsafePartial head xs
    t = unsafePartial tail xs
