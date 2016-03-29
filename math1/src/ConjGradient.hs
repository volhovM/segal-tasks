{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}

module ConjGradient (main, ConjSLAE(..)) where

import           Types                          (SolvableMatrix(..), SLAE(..))
import           Control.Lens                   (makeLenses, (.=), (+=), use)
import           Control.Monad.Loops            (untilM_)
import           Control.Monad.State.Lazy       (State, runState)
import           Numeric.LinearAlgebra          (norm_2, (<>), (<.>), (#>))
import           Numeric.LinearAlgebra.Data

data ConjState = CS {
    _xk :: Vector Double
  , _rk :: Vector Double
  , _pk :: Vector Double
}
$(makeLenses ''ConjState)

eps = 1e-9 :: Double
cs = CS (vector []) (vector []) (vector [])

type ConjSLAE = SLAE Double
type ConjSolveState a = State ConjState a

instance SolvableMatrix ConjSLAE Double where
   fromSLAE = return 
   toSLAE   = return
   rowsN    = sSize
   colsM    = sSize
   solve f  = return $ fst $ runState (conjgrad (sMatrix f) (sVector f)) cs

test_good :: Int -> (Matrix Double, Vector Double)
test_good n = (ident n, vector (map fromIntegral [1..n]))

test_hilbert :: Int -> (Matrix Double, Vector Double)
test_hilbert n = (build (n, n) (\i j -> 1/(i + j + 1)), konst 1 n)

test_dima_n :: (Matrix Double, Vector Double)
test_dima_n = (build (10, 10) gen_a, build 10 (\i -> sum (map (\j -> gen_b i j) [0..9]))) where
    gen_a i j = 10 * i + j + 1
    gen_b i j = (j + 1) * gen_a i j

conjgrad :: Matrix Double -> Vector Double -> ConjSolveState (Vector Double)
conjgrad a' b' = do
    let a = tr' a' <> a' -- A^T * A
    let b = tr' a' #> b' -- A^T * b
    xk .= (konst 0 (size b) :: Vector Double)
    rk .= b
    pk .= b
    (do pk' <- use pk
        rk' <- use rk
        let apk = a #> pk'
        let ak = rk' <.> rk' / pk' <.> apk
        let rk'' = rk' - scalar ak * apk
        let bk = rk'' <.> rk'' / rk' <.> rk'
        xk += scalar ak * pk'
        rk .= rk''
        pk .= rk'' + scalar bk * pk'
        ) `untilM_` use rk >>= return . (>) eps . norm_2
    use xk

main = do
    let (a, b) = test_good 10
    --let (a, b) = test_hilbert 10
    --let (a, b) = test_dima_n
    let x = fst $ runState (conjgrad a b) cs
    putStrLn $ show x
