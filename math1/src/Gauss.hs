module Gauss
       ( main
       ) where

import           Types         (Matrix (..), diagMatrix, get, matrix, set)

import           Control.Monad (forM, forM_)
import           Data.List     (maximumBy)
import           Data.Ord      (comparing)

-- m[i] += c * m[j]
addMul :: Matrix → Int → Double → Int → IO ()
addMul m i c j =
  forM_ [0..ncols m + freeCols m - 1] $ \k →
  do vi ← get m i k
     vj ← get m j k
     set m i k (vi + c * vj)

swapRows :: Matrix → Int → Int → IO ()
swapRows _ i j | i == j = return ()
swapRows m i j =
  forM_ [0..ncols m + freeCols m - 1] $ \k →
  do vi ← get m i k
     vj ← get m j k
     set m i k vj
     set m j k vi

columnAbsMax :: Matrix → Int → IO (Int, Double)
columnAbsMax m i = do
  l ← mapM (\j → (,) j <$> get m i j) [0..nrows m-1]
  return $ maximumBy (comparing $ abs . snd) l

elimFirstCol :: Matrix → IO Bool
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

submatrix :: Matrix → Int → Int → Matrix
submatrix m i j | nrows m < i || ncols m < j = error "submatrix"
submatrix m i j =
  m { nrows     = nrows m - i
    , ncols     = ncols m - j
    , freeCols  = freeCols m
    , rowOffset = rowOffset m + i
    , colOffset = colOffset m + j
    }

echelon :: Matrix → IO Bool
echelon m | nrows m == 0 || ncols m == 0 = return True
echelon m = do
  b ← elimFirstCol m
  if b
    then echelon (submatrix m 1 1)
    else return False

backsub :: Matrix → IO Matrix
backsub m = do
  res ← matrix (freeCols m) (ncols m) 0 (const 0) undefined
  forM_ (reverse [0..ncols m-1]) $ \i →
    forM_ [0..freeCols m-1] $ \k → do
      bik ← get m i (ncols m + k)
      mii ← get m i i
      let rik = bik/mii
      set res k i rik
      forM_ [0..i-1] $ \j → do
        bjk ← get m j (ncols m + k)
        mji ← get m j i
        set m j (ncols m + k) (bjk - mji / rik)
  return res

gauss :: Matrix → IO (Maybe Matrix)
gauss m = do
  b ← echelon m
  if b
    then Just <$> backsub m
    else return Nothing

--vectorToList :: Vector → IO [Double]
--vectorToList = getElems

matrixToList :: Matrix → IO [[Double]]
matrixToList m =
  forM [0..nrows m-1] $ \i →
    forM [0..ncols m-1] $ \j →
      get m i j

main :: IO ()
main = do
  --a ← hilbert 3
  a ← diagMatrix 5 (\i → fromIntegral i + 1)
  r ← gauss a
  case r of
    Nothing → putStrLn "singular"
    Just c  → do
      l ← matrixToList c
      print l
