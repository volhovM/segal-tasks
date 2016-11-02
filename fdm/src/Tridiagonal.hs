module Tridiagonal (solve) where

import           Control.Lens (view, _1, _2, _3, _4)
import qualified Data.Vector  as V

solve :: Int -> (Int -> (Double, Double, Double, Double)) -> V.Vector Double
solve n f = generate n x
  where
    c', d', x :: Int -> Double
    x i
        | i == n - 1 = d' (n - 1)
        | otherwise  = d' i - c' i * x (i + 1)
    d' i
        | i == 0    = d 0 / b 0
        | otherwise = (d i - a i * d' (i - 1)) / (b i - a i * c' (i - 1))
    c' i
        | i == 0    = c 0 / b 0
        | otherwise = c i / (b i - a i * c' (i - 1))
    a, b, c, d :: Int -> Double
    a = view _1 . f
    b = view _2 . f
    c = view _3 . f
    d = view _4 . f
