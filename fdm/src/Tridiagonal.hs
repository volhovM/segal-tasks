module Tridiagonal (solve) where

import           Control.Lens (view, _1, _2, _3, _4)
import qualified Data.Vector  as V

solve :: Int -> (Int -> (Double, Double, Double, Double)) -> V.Vector Double
solve n f = undefined
  where
    a, b, c, d :: Int -> Double
    a = view _1 . f
    b = view _2 . f
    c = view _3 . f
    d = view _4 . f
