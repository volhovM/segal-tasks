{-# LANGUAGE FlexibleContexts      #-}
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
--       , printMatrix
       ) where

import           Control.Monad              (forM_)
import           Foreign.Storable           (Storable)
import           Numeric.LinearAlgebra      (Field)
import           Numeric.LinearAlgebra.Data (Matrix, Vector, (><), (|>))
import           System.IO                  (stdout)
import           Text.Printf                (PrintfArg, hPrintf)

data SLAE f = SLAE
    { sSize   :: Int
    , sMatrix :: Matrix f
    , sVector :: Vector f
    }

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

--printMatrix :: (SolvableMatrix a) => a -> IO ()
--printMatrix m =
--  forM_ [0..rowsN m - 1] $ \i -> do
--    forM_ [0..colsM m - 1] $ \j -> do
--      hPrintf stdout "%.3f\t" $ M.getElem i j $ toSLAE m
--    putStrLn ""
