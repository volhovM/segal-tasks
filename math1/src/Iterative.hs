{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Iterative
    ( IterativeSolvableMatrix(..)
    , Jacobi(..)
    , Seidel(..)
    , jacobi
    ) where

import           Types                          (SolvableMatrix(..), SLAE(..))
import           Control.Lens                   (makeLenses, (.=), (+=), use)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad                  (forM_)
import           Control.Monad.Loops            (untilM_)
import           Control.Monad.State.Lazy       (StateT, runStateT)
import           Numeric.LinearAlgebra          ( add
                                                , norm_2
                                                , (<>)
                                                , (#>)
                                                , inv)
import           Numeric.LinearAlgebra.Devel    (mapMatrixWithIndex)
import           Numeric.LinearAlgebra.Data
import           Data.Array.IO                  (IOUArray, newListArray, readArray, writeArray, getElems)
import           Data.IORef                     (newIORef, modifyIORef', readIORef)

eps :: Double
eps = 1e-6

data IterationState = IS
    { _x_k   :: Vector Double
    , _x_k1  :: Vector Double
    , _iters :: Int
    }

zeroState :: Int -> IterationState
zeroState n = IS zeroVec zeroVec 0
    where zeroVec = konst 0 n

$(makeLenses ''IterationState)

class SolvableMatrix a Double => IterativeSolvableMatrix a where
    iteration :: a -> Vector Double -> IO (Vector Double)
    converges :: a -> Vector Double -> Vector Double -> IO (Bool)
    mySize :: a -> Int

    runMethod :: Int -> a -> IO (Vector Double)
    runMethod maxIters a = do
      (_, state) <- runStateT solver $ zeroState $ mySize a
      return $ _x_k1 state
        where solver :: StateT IterationState IO ()
              solver = makeStep `untilM_` convergence
              makeStep :: StateT IterationState IO ()
              makeStep = do
                x_k'  <- use x_k1
                x_k1' <- liftIO $ iteration a x_k'
                x_k1  .= x_k1'
                x_k   .= x_k'
                iters += 1
              convergence :: StateT IterationState IO Bool
              convergence = do
                it <- use iters
                if it > maxIters
                then do
                  x_k1 .= konst (0/0) (mySize a)
                  return True
                else do
                  x_k'  <- use x_k
                  x_k1' <- use x_k1
                  liftIO $ converges a x_k' x_k1'

data Jacobi = Jacobi
    { jSize    :: !Int
    , jMatrixA :: Matrix Double
    , jMatrixB :: Matrix Double
    , jNormB   :: !Double
    , jVecG    :: Vector Double
    }

instance SolvableMatrix Jacobi Double where
    fromSLAE (SLAE n jA b) = do
      let d  = (-1) * (inv $ diag $ takeDiag jA)
          lu = mapMatrixWithIndex (\(i, j) x -> if i == j then 0 else x) jA
          jB = d <> lu
          jN = norm_2 jB
          jG = (-1) * (d #> b)
      return $ Jacobi n jA jB jN jG

    toSLAE (Jacobi n jA _ _ jG) = do
      let d = diag $ takeDiag jA
      return $ SLAE n jA (d #> jG)
    rowsN = jSize
    colsM = jSize
    solve = runMethod 10000

instance IterativeSolvableMatrix Jacobi where
    converges (Jacobi _ _ _ q _) x0 x1 = return $ ((norm_2 $ x0 `add` ((-1) * x1)) / (abs $ 1 - q)) < eps
    iteration (Jacobi _ _ b _ g) x = return $ (b #> x) `add` g
    mySize = jSize

data Seidel = Seidel
    { zSize    :: !Int
    , zMatrixA :: Matrix Double
    , zMatrixB :: Matrix Double
    , zNormB   :: !Double
    , zNormB2  :: !Double
    , zVecG    :: Vector Double
    }

instance SolvableMatrix Seidel Double where
    fromSLAE slae = do
      (jac :: Jacobi) <- fromSLAE slae
      let b2  = mapMatrixWithIndex (\(i, j) x -> if i < j then x else 0) $ sMatrix slae
          zB2 = norm_2 b2
      return $ Seidel (jSize jac) (jMatrixA jac) (jMatrixB jac) (jNormB jac) zB2 (jVecG jac)

    toSLAE (Seidel n zA _ _ _ zG) = do
      let d = diag $ takeDiag zA
      return $ SLAE n zA (d #> zG)

    rowsN = zSize
    colsM = zSize
    solve = runMethod 10000

instance IterativeSolvableMatrix Seidel where
    converges (Seidel _ _ _ n n2 _) x0 x1 = return $ (norm_2 $ x0 `add` ((-1) * x1)) < (abs $ 1 - n) * eps / n2
    iteration (Seidel n _ zB _ _ zG) x = do
      (x' :: IOUArray Int Double) <- newListArray (0, n - 1) $ toList x
      forM_ [0..n - 1] $ \i -> do
        sum' <- newIORef $ zG `atIndex` i
        forM_ [0..n - 1] $ \j -> do
          let c_ij = zB `atIndex` (i, j)
          x_j <- readArray x' j
          modifyIORef' sum' (+ c_ij * x_j)
        s <- readIORef sum'
        writeArray x' i s

      elts <- getElems x'
      return $ fromList elts

    mySize = zSize

jacobi :: Int -> SLAE Double -> IO (Vector Double)
jacobi n s = do
  jac <- fromSLAE s :: IO Jacobi
  runMethod n jac
