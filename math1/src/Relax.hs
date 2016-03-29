module Relax
       --( relaxSolve
       ( upper
       , lower
       ) where

import Types                      (SLAE (..))

import Control.Lens                 (makeLenses, (.=), use)
import Control.Monad.Loops          (untilM_)
import Control.Monad.State.Lazy     (State, runState)
import Numeric.LinearAlgebra.Data   (Matrix, Vector, fromList, fromRows, scalar, size, subMatrix, subVector, takesV, toList, toLists, toRows, vjoin)

data RelaxState = RS {
      _bk :: Vector Double
    , _xp :: Vector Double
    , _xk :: Vector Double
}
--makeLenses ''RelaxState

type RelaxSolveState a = State RelaxState a

--relax :: Matrix Double -> Vector Double -> RelaxSolveState (Vector Double)
--relax = relax' 2

relax' :: Double -> Matrix Double -> Vector Double -> RelaxSolveState (Vector Double)
relax' om a' b' = do
    let a = -om * divideByDiag a' :: Matrix Double
    let n = min (rows a) (cols a)
    do {
        forM [0 .. n - 1] $ \i -> do
            
    } untilM_ ()


divideByDiag :: Matrix Double -> Vector Double -> (Matrix Double, Vector Double)
divideByDiag a b  = (fromRows $ flip mapIndiced (toRows a) $ \v i -> v / scalar (diagElement a i),
                    fromList $ flip mapIndiced (toList b) $ \x i -> x / diagElement a i)
                    where diagElement a i = (head . head . toLists . subMatrix (i, i) (1, 1)) a

upper :: Matrix Double -> Matrix Double
upper m = fromRows $ flip mapIndiced (toRows m)
          $ \v i -> vjoin [(fromList . replicate (i + 1)) 0, subVector (i + 1) (size v - i - 1) v]

lower :: Matrix Double -> Matrix Double
lower m = fromRows $ flip mapIndiced (toRows m)
          $ \v i -> vjoin [subVector 0 i v , (fromList . replicate (size v - i)) 0]

mapIndiced :: (a -> Int -> b) -> [a] -> [b]
mapIndiced f l = zipWith f l [0 ..]

substV :: Int -> Double -> Vector Double -> Vector Double
substV i x v = vjoin [pre, scalar x, post]
               where [pre, _, post] = takesV [i, 1, size v - i - 1] v

takeRow :: Int -> Matrix Double -> Vector Double
takeRow i = head . drop i . toRows
