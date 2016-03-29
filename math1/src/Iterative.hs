{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Iterative
    ( IterativeSolvableMatrix(..)
    , Jacobi(..)
    , jacobi
    ) where

import           Types                          (SolvableMatrix(..), SLAE(..))
import           Control.Lens                   (makeLenses, (.=), (+=), use)
import           Control.Monad.Loops            (untilM_)
import           Control.Monad.State.Lazy       (State, runState)
import           Numeric.LinearAlgebra          ( add
                                                , norm_0
                                                , norm_1
                                                , norm_2
                                                , norm_Inf
                                                , norm_Frob
                                                , (<>)
                                                , (#>)
                                                , inv)
import           Numeric.LinearAlgebra.Devel    (mapMatrixWithIndex)
import           Numeric.LinearAlgebra.Data
import Debug.Trace

eps :: Double
eps = 1e-9

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
    iteration :: a -> Vector Double -> Vector Double
    converges :: a -> Vector Double -> Vector Double -> Bool
    mySize :: a -> Int

    runMethod :: Int -> a -> IO (Vector Double)
    runMethod maxIters a = return $ _x_k1 state
        where (_, state) = runState solver $ zeroState $ mySize a
              solver = makeStep `untilM_` convergence
              makeStep = do
                x_k1' <- use x_k1
                x_k1  .= iteration a x_k1'
                x_k   .= x_k1'
                iters += 1
              convergence = do
                it <- use iters
                if it > maxIters
                then return True
                else do
                  x_k'  <- use x_k
                  x_k1' <- use x_k1
                  return $ converges a x_k' x_k1'

data Jacobi = Jacobi
    { jSize    :: Int
    , jMatrixA :: Matrix Double
    , jMatrixB :: Matrix Double
    , jNormB   :: Double
    , jVecG    :: Vector Double
    }

instance SolvableMatrix Jacobi Double where
    fromSLAE (SLAE n jA b) = do
      let d  = (-1) * (inv $ diag $ takeDiag jA)
          lu = mapMatrixWithIndex (\(i, j) x -> if i == j then 0 else x) jA
          jB = d <> lu
          jN = norm_2 jB
          jG = d #> b
      return $ Jacobi n jA jB jN jG

    toSLAE (Jacobi n jA _ _ jG) = do
      let d = diag $ takeDiag jA
      return $ SLAE n jA (d #> jG)
    rowsN = jSize
    colsM = jSize
    solve = runMethod 10000

instance IterativeSolvableMatrix Jacobi where
    converges (Jacobi _ _ _ q _) x0 x1 = ((norm_Inf $ x0 `add` ((-1) * x1)) / (abs $ 1 - q)) < eps
    iteration (Jacobi _ _ b _ g) x = (b #> x) `add` g
    mySize = jSize

{--
instance SolvableMatrix Seidel Double where
    { sSize    :: Int
    , sMatrixC ::
    }
--}

jacobi :: Int -> SLAE Double -> IO (Vector Double)
jacobi n s = do
  jac <- fromSLAE s :: IO Jacobi
  runMethod n jac
