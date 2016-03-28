{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Types
       ( SimpleMatrix (..)
       , SolvableMatrix (..)
       , simpleMatrix
       , diagMatrix
       , hilbert
       , printMatrix
       ) where

import           Control.Monad     (forM_)
import           Data.Array.IO     (IOUArray, newListArray, readArray)
import           Data.Array.MArray (MArray (..))
import           System.IO         (stdout)
import           Text.Printf       (PrintfArg, hPrintf)

data SimpleMatrix n = SimpleMatrix
    { sSize :: !Int
    , sData :: IOUArray (Int, Int) n
    }

class (MArray IOUArray (Elem a) IO, PrintfArg (Elem a)) => SolvableMatrix a where
    type Elem a :: *
    fromMatrix :: SimpleMatrix (Elem a) -> a
    toMatrix :: a -> IOUArray (Int, Int) (Elem a)
    rowsN :: a -> Int
    colsM :: a -> Int
    solve :: a -> IO (IOUArray Int (Elem a))

simpleMatrix
    :: (MArray IOUArray n IO)
    => Int -> ((Int, Int) -> n) -> (Int -> n) -> IO (SimpleMatrix n)
simpleMatrix n f f' = do
  let ls = flip concatMap [0..n-1] $ \i → map (f . (,) i) [0..n-1] ++ [f' i]
  vs ← newListArray ((0,0), (n-1,n)) ls
  return SimpleMatrix
    { sSize     = n
    , sData     = vs
    }

diagMatrix :: Int → (Int → Double) → IO (SimpleMatrix Double)
diagMatrix n f = simpleMatrix n (\(i,j) → if i==j then f i else 0) $ const 1

hilbert :: Int → IO (SimpleMatrix Double)
hilbert n = simpleMatrix n (\(i,j) → 1 / fromIntegral (i+j+1)) $ const 1

printMatrix :: (SolvableMatrix a) => a -> IO ()
printMatrix m =
  forM_ [0..rowsN m - 1] $ \i -> do
    forM_ [0..colsM m - 1] $ \j -> do
      v <- readArray (toMatrix m) (i,j)
      hPrintf stdout "%.3f\t" v
    putStrLn ""
