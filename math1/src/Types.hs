module Types
       ( Matrix (..)
       , get
       , set
       , getIndex
       , matrix
       , diagMatrix
       , hilbert
       , printMatrix
       ) where

import           Data.Array.IO (IOUArray, newListArray, readArray, writeArray)
import           Control.Monad (forM_)
import           Text.Printf (hPrintf)
import           System.IO (stdout)

--type Vector = IOUArray Int Double

data Matrix = Matrix
    { nrows     :: !Int
    , ncols     :: !Int
    , freeCols  :: !Int
    , rowOffset :: !Int
    , colOffset :: !Int
    , vals      :: IOUArray (Int, Int) Double
    }

getIndex :: Matrix → Int → Int → (Int, Int)
getIndex m i j = (rowOffset m + i, colOffset m + j)

get :: Matrix → Int → Int → IO Double
get m i j = readArray (vals m) (getIndex m i j)

set :: Matrix → Int → Int → Double → IO ()
set m i j = writeArray (vals m) (getIndex m i j)

matrix
    :: Int
    -> Int
    -> Int
    -> ((Int, Int) -> Double)
    -> ((Int, Int) -> Double)
    -> IO Matrix
matrix n m m' f f' = do
  let ls = flip concatMap [0..n-1] $ \i → map (f . (,) i) [0..m-1] ++ map (f' . (,) i) [0..m'-1]
  vs ← newListArray ((0,0), (n-1,m+m'-1)) ls
  return Matrix
    { nrows     = n
    , ncols     = m
    , freeCols  = m'
    , rowOffset = 0
    , colOffset = 0
    , vals      = vs
    }

diagMatrix :: Int → (Int → Double) → IO Matrix
diagMatrix n f = matrix n n 1 (\(i,j) → if i==j then f i else 0.0) $ const 1

hilbert :: Int → IO Matrix
hilbert n = matrix n n 1 (\(i,j) → 1 / fromIntegral (i+j+1)) $ const 1

printMatrix :: Matrix -> IO ()
printMatrix m = do
  forM_ [0..nrows m - 1] $ \i -> do
    forM_ [0..ncols m - 1] $ \j -> do
      v <- get m i j
      hPrintf stdout "%.3f\t" v
    putStrLn ""
