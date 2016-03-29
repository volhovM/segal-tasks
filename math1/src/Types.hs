{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Types
       ( SLAE (..)
       , SolvableMatrix (..)
       , simpleMatrix
       , diagMatrix
       , hilbert
       , goodMatrix
       , cond
--       , printMatrix
       ) where

import           Foreign.Storable           (Storable)
import           Numeric.LinearAlgebra      (Element, Field, det, inv)
import           Numeric.LinearAlgebra.Data (Matrix, Vector, asColumn, cols,
                                             disps, rows, toLists, (><), (|>),
                                             (|||))

data SLAE f = SLAE
    { sSize   :: Int
    , sMatrix :: Matrix f
    , sVector :: Vector f
    }

instance Show (SLAE Double) where
    show SLAE{..} = disps 3 (sMatrix ||| asColumn sVector)

class (Field f) => SolvableMatrix a f where
    fromSLAE :: SLAE f -> IO a
    toSLAE   :: a -> IO (SLAE f)
    rowsN    :: a -> Int
    colsM    :: a -> Int
    solve    :: a -> IO (Vector f)

simpleMatrix
    :: (Storable a)
    => Int -> ((Int, Int) -> a) -> (Int -> a) -> SLAE a
simpleMatrix (n :: Int) f f' =
    let sMatrixData =
            flip concatMap [0 .. n - 1] $
            \i ->
                 map (f . (,) i) [0 .. n - 1]
        sVectorData = map f' [0 .. n - 1]
    in SLAE
       { sSize = n
       , sMatrix = (n >< n) sMatrixData
       , sVector = n |> sVectorData
       }

diagMatrix :: Int -> (Int -> Double) -> SLAE Double
diagMatrix n f = simpleMatrix n (\(i,j) -> if i==j then f i else 0) $ const 1

hilbert :: Int -> SLAE Double
hilbert n = simpleMatrix n (\(i,j) -> 1 / fromIntegral (i+j+1)) $ const 1

goodMatrix :: Int -> SLAE Double
goodMatrix n =
  flip (simpleMatrix n) (const 1) $ \(i,j) ->
  if i == j
    then 1 + sum (flip map ([0..i-1]++[i+1..n-1]) $ \k → 1 / fromIntegral (i+k+1))
    else 1 / fromIntegral (i+j+1)

cond :: (Element t, Field t, Num t, Ord t) => Matrix t -> Maybe t
cond m =
    if rows m == 0 || rows m /= cols m || det m == 0
        then Nothing
        else Just $ norm m * norm (inv m)

norm :: (Element t, Num t, Ord t) => Matrix t -> t
norm m = maximum $ flip map (toLists m) $ foldl (\i j -> i + abs j) 0
