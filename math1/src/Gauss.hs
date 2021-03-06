{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Gauss (GaussMatrix, main) where

import           Types                      (SLAE (..), SolvableMatrix (..),
                                             goodMatrix)

import           Control.Monad              (forM, forM_, when)
import           Data.Array.IO              (IOUArray, newListArray, readArray,
                                             writeArray)
import           Data.List                  (maximumBy)
import           Data.Ord                   (comparing)
import           Numeric.LinearAlgebra.Data (asColumn, matrix, toLists, vector,
                                             (|||))

data GaussMatrix = GaussMatrix
    { nrows     :: !Int
    , ncols     :: !Int
    , freeCols  :: !Int
    , rowOffset :: !Int
    , colOffset :: !Int
    , vals      :: IOUArray (Int, Int) Double
    }

instance SolvableMatrix GaussMatrix Double where
    fromSLAE SLAE{..} = do
        vals <-
            newListArray ((0, 0), (sSize - 1, sSize)) $
            concat $ toLists (sMatrix ||| asColumn sVector)
        return
            GaussMatrix
            { nrows = sSize
            , ncols = sSize
            , freeCols = 1
            , rowOffset = 0
            , colOffset = 0
            , ..
            }
    toSLAE m@GaussMatrix{..} = do
      when (nrows /= ncols) $ error "gauss: nrows /= ncols"
      when (freeCols /= 1) $ error "gauss: freeCols /= 1"
      when (rowOffset /= 0 || colOffset /= 0) $ error "gauss: offset"

      let sSize = nrows
      sMatrix ← matrix sSize . concat <$> matrixToList m
      sVector ← vector <$> forM [0..sSize-1] (\i → get m i sSize)

      return SLAE{..}
    rowsN = nrows
    colsM = ncols
    solve m = do
      Just r ← gauss m
      vector <$> forM [0..ncols r-1] (\i → get r 0 i)

getIndex :: GaussMatrix → Int → Int → (Int, Int)
getIndex m i j = (rowOffset m + i, colOffset m + j)

get :: GaussMatrix → Int → Int → IO Double
get m i j = readArray (vals m) (getIndex m i j)

set :: GaussMatrix → Int → Int → Double → IO ()
set m i j = writeArray (vals m) (getIndex m i j)

gmatrix
    :: Int
    → Int
    → Int
    → ((Int, Int) → Double)
    → ((Int, Int) → Double)
    → IO GaussMatrix
gmatrix n m m' f f' = do
  let ls = flip concatMap [0..n-1] $ \i → map (f . (,) i) [0..m-1] ++ map (f' . (,) i) [0..m'-1]
  vs ← newListArray ((0,0), (n-1,m+m'-1)) ls
  return GaussMatrix
    { nrows     = n
    , ncols     = m
    , freeCols  = m'
    , rowOffset = 0
    , colOffset = 0
    , vals      = vs
    }

-- m[i] += c * m[j]
addMul :: GaussMatrix → Int → Double → Int → IO ()
addMul m i c j =
  forM_ [0..ncols m + freeCols m - 1] $ \k →
  do vi ← get m i k
     vj ← get m j k
     set m i k (vi + c * vj)

swapRows :: GaussMatrix → Int → Int → IO ()
swapRows _ i j | i == j = return ()
swapRows m i j =
  forM_ [0..ncols m + freeCols m - 1] $ \k →
  do vi ← get m i k
     vj ← get m j k
     set m i k vj
     set m j k vi

columnAbsMax :: GaussMatrix → Int → IO (Int, Double)
columnAbsMax m i = do
  l ← mapM (\j → (,) j <$> get m j i) [0..nrows m-1]
  return $ maximumBy (comparing $ abs . snd) l

elimFirstCol :: GaussMatrix → IO Bool
elimFirstCol m = do
  (imax, vmax) ← columnAbsMax m 0
  if vmax == 0 then
    return False
  else do
    swapRows m 0 imax
    forM_ [1..nrows m-1] $ \j → do
      vj0 ← get m j 0
      let c = - vj0 / vmax
      addMul m j c 0
      set m j 0 0.0 -- just to be sure
    return True

submatrix :: GaussMatrix → Int → Int → GaussMatrix
submatrix m i j | nrows m < i || ncols m < j = error "submatrix"
submatrix m i j =
  m { nrows     = nrows m - i
    , ncols     = ncols m - j
    , freeCols  = freeCols m
    , rowOffset = rowOffset m + i
    , colOffset = colOffset m + j
    }

echelon :: GaussMatrix → IO Bool
echelon m | nrows m == 0 || ncols m == 0 = return True
echelon m = do
  b ← elimFirstCol m
  if b
    then echelon (submatrix m 1 1)
    else return False

backsub :: GaussMatrix → IO GaussMatrix
backsub m = do
  res ← gmatrix (freeCols m) (ncols m) 0 (const 0) undefined
  forM_ (reverse [0..nrows m-1]) $ \i →
    forM_ [0..freeCols m-1] $ \k → do
      bik ← get m i (ncols m + k)
      mii ← get m i i
      let rik = bik/mii
      set res k i rik
      forM_ [0..i-1] $ \j → do
        bjk ← get m j (ncols m + k)
        mji ← get m j i
        set m j (ncols m + k) (bjk - mji * rik)
  return res

gauss :: GaussMatrix → IO (Maybe GaussMatrix)
gauss m = do
  b ← echelon m
  if b
    then Just <$> backsub m
    else return Nothing

--vectorToList :: Vector → IO [Double]
--vectorToList = getElems

matrixToList :: GaussMatrix → IO [[Double]]
matrixToList m =
  forM [0..nrows m-1] $ \i →
    forM [0..ncols m-1] $ \j →
      get m i j

main :: IO ()
main = do
  a ← fromSLAE $ goodMatrix 3
  -- a ← fromSLAE $ hilbert 3
  -- a ← fromSLAE $ diagMatrix 5 (\i → fromIntegral i + 1)
  print =<< matrixToList a
  r ← gauss a
  case r of
    Nothing → putStrLn "singular"
    Just c  → do
      l ← matrixToList c
      print l
