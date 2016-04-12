{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Iterative
    ( IterativeSolvableMatrix(..)
    , Jacobi(..)
    , Seidel(..)
    , Relax(..)
    , jacobi
    , seidel
    , relax
    ) where

import           Control.Lens                (makeLenses, use, (+=), (.=))
import           Control.Monad               (forM_)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Loops         (untilM_)
import           Control.Monad.State.Lazy    (StateT, runStateT)
import           Data.Array.IO               (IOUArray, getElems, newListArray,
                                              readArray, writeArray)
import           Data.IORef                  (modifyIORef', newIORef, readIORef)
import           Numeric.LinearAlgebra       (add, inv, norm_2, ( #> ), (<>))
import           Numeric.LinearAlgebra.Data
import           Numeric.LinearAlgebra.Devel (mapMatrixWithIndex)
import           Types                       (SLAE (..), SolvableMatrix (..))

eps :: Double
eps = 1e-6

maxIters :: Int
maxIters = 10000

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
    converges :: a -> Vector Double -> Vector Double -> IO Bool
    mySize :: a -> Int

    runMethod :: Int -> a -> IO (Vector Double)
    runMethod maxIterations a = do
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
                if it > maxIterations
                then do
                  x_k1 .= konst 228 (mySize a)
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
    solve = runMethod maxIters

instance IterativeSolvableMatrix Jacobi where
    converges (Jacobi _ _ _ q _) x0 x1 =
        return $ (norm_2 (x0 `add` ((-1) * x1)) / abs (1 - q)) < eps
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
    solve = runMethod maxIters

instance IterativeSolvableMatrix Seidel where
    converges (Seidel _ _ _ n n2 _) x0 x1 =
        return $ (norm_2 $ x0 `add` ((-1) * x1)) < (abs $ 1 - n) * eps / n2
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

data Relax = Relax
    { rOm       :: !Double
    , rSize     :: !Int
    , rMatrixA  :: Matrix Double
    , rMatrixA' :: Matrix Double
    , rVecB'    :: Vector Double
    , rNormA'   :: !Double
    , rNormU'   :: !Double
    }

--runMethodForConvergence :: Int -> Relax -> IO Double
--runMethodForConvergence maxIters a = do
--  (_, state) <- runStateT solver $ zeroState $ mySize a
--  return $ norm_2 $ _x_k state `add` ((-1) * _x_k1 state)
--    where solver :: StateT IterationState IO ()
--          solver = makeStep `untilM_` convergence
--          makeStep :: StateT IterationState IO ()
--          makeStep = do
--            x_k'  <- use x_k1
--            x_k1' <- liftIO $ iteration a x_k'
--            x_k1  .= x_k1'
--            x_k   .= x_k'
--            iters += 1
--          convergence :: StateT IterationState IO Bool
--          convergence = do
--            it <- use iters
--            return $ it > maxIters

--findOmega :: Int -> Matrix Double -> Matrix Double -> Vector Double -> Relax
--findOmega n a a0 b0 = do
--    let maxN = 50
--        omEps = 0.01
--    (l, leps) <- getEps 1.0
--    (m, meps) <- getEps 1.5
--    (r, reps) <- getEps 2.0
--    do
--        if (leps < meps || meps <= reps && leps < reps)
--        then do
--            modifyIORef r m
--            modifyIORef reps meps
--        else do
--            modifyIORef l m
--            modifyIORef leps meps
--            m'
--        getEps (modifyEps l r) >>= \(m'', eps'') -> do
--            modifyIORef m m''
--            modifyIORef meps eps''
--        `whileM_` (return $ (m - l < omEps) || (r - m < omEps))
--    if (meps < leps)
--    then do
--        feps <- newIORef =<< readIORef meps
--        om <- newIORef =<< readIORef m
--    else do
--        feps <- newIORef =<< readIORef leps
--        om <- newIORef =<< readIORef l
--    if (reps < feps)
--    then return $ placeOm om n a a0 b0
--    else return $ placeOm r n a a0 b0
--        where getEps :: Double -> (IORef Double, IORef Double)
--              getEps om = do
--                eps' <- runMethodForConvergence maxN $ placeOm om n a a0 b0
--                return (newIORef om, newIORef eps')
--              placeOm :: Double -> Int -> Matrix Double -> Matrix Double -> Vector Double -> Relax
--              placeOm om n a a0 b0 = Relax om n a a' b' na' nu'
--                where a' = scalar (-om) * a0 + ident n
--                      b' = scalar om * b0
--                      na' = norm_2 a'
--                      nu' = norm_2 $ mapMatrixWithIndex (\(i, j) x -> if i < j then x else 0) a'
--              modifyEps :: Double -> Double -> IO Double
--              modifyEps l r = readIORef l >>= readIORef r >>= return . (*) 0.5 . (+)

instance SolvableMatrix Relax Double where
    fromSLAE (SLAE n a0 b0) = do
        let om = 1.8
            d = diag $ takeDiag a0
            a = scalar (-om) * inv d <> a0 + ident n
            b = scalar om * inv d #> b0
            u = mapMatrixWithIndex (\(i, j) x -> if i < j then x else 0) a
            na = norm_2 a
            nu = norm_2 u
        return $ Relax om n a0 a b na nu
--        let d = diag $ takeDiag a
--            a' = inv d <> a
--            b' = inv d #> b
--        return $ findOmega n a a' b'

    toSLAE (Relax om n a _ b' _ _) = do
        let d = diag $ takeDiag a
        return $ SLAE n a (d #> b' / scalar om)

    rowsN = rSize
    colsM = rSize
    solve = runMethod maxIters

instance IterativeSolvableMatrix Relax where
    converges (Relax _ n a a' b' na' nu') = converges (Seidel n a a' na' nu' b')
    iteration (Relax _ n a a' b' na' nu') = iteration (Seidel n a a' na' nu' b')
    mySize = rSize

jacobi :: Int -> SLAE Double -> IO (Vector Double)
jacobi n s = do
  jac <- fromSLAE s :: IO Jacobi
  runMethod n jac

seidel :: Int -> SLAE Double -> IO (Vector Double)
seidel n s = do
    sei <- fromSLAE s :: IO Seidel
    runMethod n sei

relax :: Int -> SLAE Double -> IO (Vector Double)
relax n s = do
    rel <- fromSLAE s :: IO Relax
    runMethod n rel
