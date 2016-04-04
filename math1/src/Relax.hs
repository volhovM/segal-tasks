{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Relax
       ( RelaxSLAE (..)
       , main
       ) where

import           Types                      (SLAE (..), SolvableMatrix (..),
                                             goodMatrix)

import           Control.Lens               (makeLenses, use, (+=), (.=))
import           Control.Monad              (forM_)
import           Control.Monad.Loops        (untilM_)
import           Control.Monad.State.Lazy   (State, runState)
import           Numeric.LinearAlgebra      (norm_2, (<.>))
import           Numeric.LinearAlgebra.Data

data RelaxState = RS
    { _rk :: Vector Double
    , _xk :: Vector Double
    } deriving (Show)

$(makeLenses ''RelaxState)

eps :: Double
eps = 1e-9 :: Double

inf :: Double
inf = 1e+9 :: Double

rs :: RelaxState
rs = RS (vector []) (vector [])

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
relax = relax' 1.8

relax' :: Double -> Matrix Double -> Vector Double -> RelaxSolveState (Vector Double)
relax' om a' b' = do
    let n = min (rows a') (cols a')
    let (a, b) = (-scalar om * (a'' - ident n), scalar om * b'') where (a'', b'') = divideByDiag a' b'
    xk .= konst 0.0 n
    do
        rk .= konst 0.0 n
        forM_ [0 .. n - 1] $ \i -> do
            xk' <- use xk
            let newXi = (1 - om) * xk' `atIndex` i + b `atIndex` i + takeRow i a <.> xk'
            let xiDiff = substV i (newXi - xk' `atIndex` i) $ konst 0.0 n
            rk += xiDiff
            xk += xiDiff
        `untilM_` use rk >>= \r -> use xk >>= \x -> return $ (eps > norm_2 r) || (norm_2 x > inf)
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
    m <- fromSLAE $ goodMatrix 3 :: IO RelaxSLAE
    --m <- fromSLAE $ hilbert 3 :: IO RelaxSLAE
    print =<< return m
    print =<< return . toList =<< (solve m :: IO (Vector Double))
