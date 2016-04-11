{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Relax
       ( RelaxSLAE (..)
       , main
       ) where

import           Types                      (SLAE (..), SolvableMatrix (..),
                                             goodMatrix, hilbert)

import           Control.Lens               (makeLenses, use, (+=), (.=))
import           Control.Monad              (forM_)
import           Control.Monad.Loops        (untilM_)
import           Control.Monad.State.Lazy   (State, runState)
import           Numeric.LinearAlgebra      (norm_2, (<.>))
import           Numeric.LinearAlgebra.Data

data RelaxState = RS
    { _rk :: Vector Double
    , _xk :: Vector Double
    , _k  :: Int
    } deriving (Show)

$(makeLenses ''RelaxState)

eps :: Double
eps = 1e-9 :: Double

rs :: RelaxState
rs = RS (vector []) (vector []) 0

newtype RelaxSLAE = RelaxSLAE { fromRelaxSLAE :: SLAE Double } deriving (Show)
type RelaxSolveState a = State RelaxState a

instance SolvableMatrix RelaxSLAE Double where
    fromSLAE = return . RelaxSLAE
    toSLAE = return . fromRelaxSLAE
    rowsN = sSize . fromRelaxSLAE
    colsM = sSize . fromRelaxSLAE
    solve f =
        return $
        fst $
        runState
            (relax (sMatrix $ fromRelaxSLAE f) (sVector $ fromRelaxSLAE f))
            rs

relax :: Matrix Double -> Vector Double -> RelaxSolveState (Vector Double)
relax = relax' 10000 1.8

relax' :: Int -> Double -> Matrix Double -> Vector Double -> RelaxSolveState (Vector Double)
relax' maxSteps om a' b' = do
    let n = min (rows a') (cols a')
    let (a, b) = (-scalar om * (a'' - ident n), scalar om * b'') where (a'', b'') = divideByDiag a' b'
    k .= 0
    xk .= konst 0.0 n
    do
        k += 1
        rk .= konst 0.0 n
        forM_ [0 .. n - 1] $ \i -> do
            xk' <- use xk
            let newXi = (1 - om) * xk' `atIndex` i + b `atIndex` i + takeRow i a <.> xk'
            let xiDiff = substV i (newXi - xk' `atIndex` i) $ konst 0.0 n
            rk += xiDiff
            xk += xiDiff
        i <- use k
        if i > maxSteps
        then do
            xk .= konst 0.0 n
        else return ()
        `untilM_` (use rk >>= \r -> use k >>= \i -> return $ (eps > norm_2 r) || (i > maxSteps))
    use xk

divideByDiag :: Matrix Double -> Vector Double -> (Matrix Double, Vector Double)
divideByDiag a b  = (fromRows $ flip mapIndiced (toRows a) $ \v i -> v / scalar (diagElement a i),
                    fromList $ flip mapIndiced (toList b) $ \x i -> x / diagElement a i)
                    where diagElement _ i = (head . head . toLists . subMatrix (i, i) (1, 1)) a

mapIndiced :: (a -> Int -> b) -> [a] -> [b]
mapIndiced f l = zipWith f l [0 ..]

substV :: Int -> Double -> Vector Double -> Vector Double
substV i x v = vjoin [pre, scalar x, post]
               where [pre, _, post] = takesV [i, 1, size v - i - 1] v

takeRow :: Int -> Matrix Double -> Vector Double
takeRow i = head . drop i . toRows

main :: IO ()
main = do
    --m <- fromSLAE $ goodMatrix 3 :: IO RelaxSLAE
    m <- fromSLAE $ hilbert 5 :: IO RelaxSLAE
    print =<< return m
    print =<< return . toList =<< (solve m :: IO (Vector Double))
