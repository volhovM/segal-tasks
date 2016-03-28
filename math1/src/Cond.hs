module Cond
       ( cond
       ) where

import Numeric.LinearAlgebra      (Element, Field, det, inv)
import Numeric.LinearAlgebra.Data (Matrix, cols, rows, toLists)

cond :: (Element t, Field t, Num t, Ord t) => Matrix t -> Maybe t
cond m =    if rows m == 0 || rows m /= cols m || det m == 0
            then Nothing
            else Just $ norm m * (norm $ inv m)

norm :: (Element t, Num t, Ord t) => Matrix t -> t
norm m = foldl1 max $ flip map (toLists m) $ foldl (\i j -> i + abs j) 0
