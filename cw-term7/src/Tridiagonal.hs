{-# LANGUAGE Strict #-}
module Tridiagonal (solve) where

import           Control.Lens                (view, _1, _2, _3, _4)
import           Control.Monad               (forM_)
import           Data.Vector.Generic         ((!))
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

solve :: Int → (Int → (Double, Double, Double, Double)) → VU.Vector Double
solve n f = x
  where
    cd :: VU.Vector (Double, Double)
    cd = iterateNWithIndex n
      (\i (cp,dp) → ( c i / (b i - a i * cp)
                    , (d i - a i * dp) / (b i - a i * cp)
                    ))
      (c 0 / b 0, d 0 / b 0)

    x :: VU.Vector Double
    x = iterateNBackWithIndex n (\i xp → let (c',d') = cd!i in d' - c' * xp) (snd $ cd!(n-1))

    iterateNWithIndex :: VUM.Unbox a ⇒ Int → (Int → a → a) → a → VU.Vector a
    iterateNWithIndex n f a = VU.create $ do
      res <- VUM.new n
      VUM.write res 0 a
      forM_ [1..n-1] $ \i → VUM.write res i =<< f i <$> VUM.read res (i-1)
      pure res

    iterateNBackWithIndex :: VUM.Unbox a ⇒ Int → (Int → a → a) → a → VU.Vector a
    iterateNBackWithIndex n f a = VU.create $ do
      res <- VUM.new n
      VUM.write res (n-1) a
      forM_ [n-2,n-3..0] $ \i → VUM.write res i =<< f i <$> VUM.read res (i+1)
      pure res

    a, b, c, d :: Int → Double
    a = view _1 . f
    b = view _2 . f
    c = view _3 . f
    d = view _4 . f
