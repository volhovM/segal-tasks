{-# LANGUAGE RankNTypes #-}
module Tridiagonal (solve) where

import           Control.Applicative (liftA2)
import           Control.Lens        (view, _1, _2, _3, _4)
import           Control.Monad.ST    (ST, runST)
import           Control.Monad.State (State, StateT, evalStateT, gets, modify)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

instance (Num a, Monad m) => Num (StateT s m a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  fromInteger = pure . fromInteger
  abs = undefined
  signum = undefined

instance (Fractional a, Monad m) => Fractional (StateT s m a) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)


type Field s = VM.STVector s (Maybe Double)
data Mem s = Mem { _f1, _f2, _f3 :: Field s }
type St s a = StateT (Mem s) (ST s) a
type Lens s = (Mem s -> Field s, Mem s -> Field s -> Mem s)

f1, f2, f3 :: Lens s
f1 = (\Mem{..} -> _f1, \Mem{..} -> \_f1 -> Mem{..})
f2 = (\Mem{..} -> _f2, \Mem{..} -> \_f2 -> Mem{..})
f3 = (\Mem{..} -> _f3, \Mem{..} -> \_f3 -> Mem{..})

use :: Lens s -> St s (Field s)
use (f,_) = gets f

assign :: Lens s -> Field s -> St s ()
assign (_,g) x = modify (\m -> g m x)


evalSt :: Int -> (forall s. St s a) -> a
evalSt n s = runST (evalStateT s =<< (Mem <$> v <*> v <*> v))
  where
    v :: ST s (Field s)
    v = VM.replicate n Nothing

solve :: Int -> (Int -> (Double, Double, Double, Double)) -> V.Vector Double
solve n f = evalSt n $ V.generateM n x
  where
    c', d', x :: Int -> St s Double
    x = memoize f1 $ \i -> if
        | i == n - 1 -> d' (n - 1)
        | otherwise  -> d' i - c' i * x (i + 1)
    d' = memoize f2 $ \i -> if
        | i == 0    -> d 0 / b 0
        | otherwise -> (d i - a i * d' (i - 1)) / (b i - a i * c' (i - 1))
    c' = memoize f3 $ \i -> if
        | i == 0    -> c 0 / b 0
        | otherwise -> c i / (b i - a i * c' (i - 1))

    memoize :: Lens s -> (Int -> St s Double) -> Int -> St s Double
    memoize x f i = do
      vx <- use x
      vval <- VM.read vx i
      case vval of
        Nothing -> do
          val <- f i
          VM.write vx i (Just val)
          --let vx' = (V.//) vx [(i, Just val)]
          -- assign x vx'
          pure val
        Just r  -> pure r

    a, b, c, d :: Int -> St s Double
    a = pure . view _1 . f
    b = pure . view _2 . f
    c = pure . view _3 . f
    d = pure . view _4 . f
