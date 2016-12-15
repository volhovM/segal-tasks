{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict         #-}
module Solve where

import           Control.Monad                    (forM_, when)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import           Data.Bool                        (bool)
import           Data.Function                    (fix)
import           Data.List                        (intercalate)
import qualified Data.Vector                      as V
import           Data.Vector.Generic              ((!))
import qualified Data.Vector.Generic              as VG
import qualified Data.Vector.Mutable              as VM
import qualified Data.Vector.Unboxed              as VU
import qualified Data.Vector.Unboxed.Mutable      as VUM
import           Numeric                          (showEFloat, showFFloat)
import           System.IO                        (hSetEncoding, stdout, utf8)


import qualified Tridiagonal                      (solve)
import           Types

-- X, T
type Val = (VU.Vector Double, VU.Vector Double)
type MVal = (VUM.IOVector Double, VUM.IOVector Double)

mVal :: Val → IO MVal
mVal (xx, tt) = (,) <$> VU.thaw xx <*> VU.thaw tt

iterateN :: Int → (a → a) → (a → a)
iterateN n f a = (!! n) $ iterate f a

explicitStep :: Pars → Val → Val
explicitStep Pars{..} (xx,tt) =
    ( explicitly 0 $ \i →
         r_xx         * xx!(i-1)
       + (1 - 2*r_xx) * xx!i
       + r_xx         * xx!(i+1)
       - dt * ww (xx!i) (tt!i)
    , explicitly ttm $ \i →
         r_tt       * tt!(i-1)
       + (1-2*r_tt) * tt!i
       + r_tt       * tt!(i+1)
       + qq/cc * dt * ww (xx!i) (tt!i)
    )
  where
    explicitly :: Double → (Int → Double) → VU.Vector Double
    explicitly l f =
        VU.generate (ii+1) $ \i →
        if | i == 0  → l
           | i == ii → f (ii-1)
           | True    → f i

implicitStep :: Int → Pars → Val → Val
implicitStep nsteps Pars{..} val@(xxn,ttn) =
    --predictCorrect step val
    iterateN nsteps step val
  where
    predictCorrect :: (a → a) → (a → a)
    predictCorrect = iterateN 2

    step :: Val → Val
    step (xx, tt) =
      ( implicitly 0 $ \i → (-r_xx, 1 + 2*r_xx, -r_xx, xxn!i - dt * ww' i)
      , implicitly ttm $ \i → (-r_tt, 1 + 2*r_tt, -r_tt, ttn!i + dt * qq/cc * ww' i)
      ) where ww' i = ww (xx!i) (tt!i)

    implicitly :: Double → (Int → (Double, Double, Double, Double)) → VU.Vector Double
    implicitly l f =
        Tridiagonal.solve (ii+1) $ \i →
        if | i == 0  → (9999, 1, 0, l)
           | i == ii → (1, -1, 9999, 0)
           | True    → f i

  {-
rarefy :: VG.Vector v a ⇒ Int → v a → v a
rarefy k v =
  let n = VG.length v
      r = n `div` k
  in VG.generate k $ \i → v!(i*r)
  --in VG.generate k $ \i → if (i<=5) then (VG.last v) else (v!(i*r))
  --in VG.generate k $ \i → if (i<=5) then (v!((k-1)*r)) else (v!(i*r))
-}

vcount :: VU.Unbox a ⇒ (a → Bool) → VU.Vector a → Int
vcount p v = VU.sum $ flip VU.map v $ \x → bool 0 1 (p x)

runMethod :: Pars → Int → Int → IO (VU.Vector Double, VU.Vector Double)
runMethod pars@Pars{method, ww, dt, ii} n k =
  do
    xxr ← VUM.new (n*(ii+1))
    ttr ← VUM.new (n*(ii+1))
    flip evalStateT (initial pars) $
      forM_ [0..n-1] $ \i → do
        (xx, tt) ← vnext' i
        liftIO $ VUM.copy (VUM.slice (i*(ii+1)) (ii+1) xxr) xx
        liftIO $ VUM.copy (VUM.slice (i*(ii+1)) (ii+1) ttr) tt
    (,) <$> VU.freeze xxr <*> VU.freeze ttr
  where
    vnext = case method of
      Explicit   → explicitStep
      Implicit n → implicitStep n

    vnext' :: Int → StateT Val IO MVal
    vnext' i = do
      cur@(xx,tt) <- get
      let topr = intercalate "   "
                 [ show i ++ "/" ++ show n
                 --, "X = " ++ show (xx'!1)
                 --, "T = " ++ show (tt'!1)
                 --, "w = " ++ show (w'!1)
                 , "X = " ++ show (VG.last xx)
                 , "T = " ++ show (VG.last tt)
                 -- , "W = " ++ show (V.last ww')
                 --, "nanX " ++ show (vcount isNaN xx)
                 --, "infX " ++ show (vcount isInfinite xx)
                 --, "nanT " ++ show (vcount isNaN tt)
                 --, "infT " ++ show (vcount isInfinite tt)
                 ]
      let next = iterateN k (vnext pars) cur
      when (i `mod` 10 == 0) $ liftIO $ putStrLn topr
      put next
      liftIO $ mVal (xx, tt)

initial :: Pars → Val
initial Pars{ii} =
    ( VU.generate (ii+1) $ \i → if i == 0 then 0 else 1
    , VU.generate (ii+1) $ \i → if i == 0 then ttm else tt0
    )


pars0 :: Pars
pars0 = Pars{..}
  where
    method = Implicit 2
    --method = Explicit

    --α = 0.5
    --α = 0.8
    --α = 0.95
    α = 1
    --α = 2
    --α = 3

    --kk = 1.6e6
    --kk = 5e6
    kk = 1e7
    --kk = 1e8

    --dd = 8e-12
    --dd = 8e-10
    --dd = 1e-10
    --dd = 1e-9
    dd = λ / (ρ*cc)

    --ee = 8e4
    --ee = 9e4
    --ee = 1e5
    --ee = 1.1e5
    --ee = 2e5
    -- kk = kk' * exp ((ee - ee') / (rr*ttm))
    {-
     to keep `K*exp(-E/(R*Tₘ)` constant:
     K * exp(−E/(RT)) = K' * exp(−E'/(RT))
     exp(−E/(RT)) / exp(−E'/(RT)) = K'/K
     exp((E'−E)/(RT)) = K'/K
     (E'−E)/(RT) = log(K'/K)
     E'−E = RT*log(K'/K)
     E' = E + RT*log(K'/K)
    -}
    ee = ee' + rr * ttm * log (kk / kk')
      where kk' = 1.6e6
            ee' = 8e4

    dz = δw * 0.9
    --dz = δw
    --dz = δw * 0.5
    --dz = δw * 0.1
    --dt = dz/u * 0.0005
    --dt = dz/u * 0.001
    dt = dz/u * 0.01
    --dt = dz/u * 0.01
    --dt = dz/u * 0.05
    --dt = dz/u * 0.1
    --ii = round (δt * 10 / dz)
    --ii = round (δt * 15 / dz)
    ii = round (3e-2 / dz)
    --ii = round (1e-2 / dz)
    --ii = round (δt * 20 / dz)
    --ii = round (δt * 30 / dz)
    --nn = round $ h/(u*dt) / 2
    --nn = round $ h/(u*dt) / 1.3
    nn = round $ h/(u*dt)
    --nn = round $ h/(u*dt) * 1.5
    --nn = round $ h/(u*dt) * 2.3
    --nn = round $ h/(u*dt) * 5

    h = fromIntegral ii * dz

    ww = \xx tt → kk * xx**α * exp (-ee / (rr*tt))
    r_xx = dd * dt/dz**2
    r_tt = λ/(ρ*cc) * dt/dz**2

    u = sqrt $ 2*kk*λ / (qq*ρ*(ttm - tt0)) * (rr*ttm**2/ee)**2 * exp(-ee/(rr*ttm))
    δt = λ / (ρ*cc*u)
    β = rr*ttm/ee
    δw = δt*β

ss :: IO ()
ss = do
  hSetEncoding stdout utf8
  let showVal (s, v) = putStrLn (s ++ " = " ++ showE 2 v)
  putStrLn "values:"
  putStrLn $ "α = " ++ showE 2 (α pars0)
  putStrLn $ "K = " ++ showE 2 (kk pars0)
  putStrLn $ "D = " ++ showE 2 (dd pars0)
  putStrLn $ "E = " ++ showE 2 (ee pars0)
  putStrLn ""
  putStrLn $ "u  = " ++ showE 2 (u pars0)
  putStrLn $ "δt = " ++ showE 2 (δt pars0)
  -- putStrLn $ "β  = " ++ showE 2 (β pars0)
  putStrLn $ "δw = " ++ showE 2 (δw pars0)
  putStrLn ""
  putStrLn $ "dz = " ++ showE 2 (dz pars0)
  putStrLn $ "dt = " ++ showE 2 (dt pars0)
  putStrLn $ "I = " ++ show (ii pars0)
  putStrLn $ "N = " ++ show (nn pars0)
  putStrLn ""
  putStrLn $ "h = " ++ showE 2 (h pars0)
  putStrLn $ "dz/u = " ++ showF 2 (dz pars0 / u pars0)
  putStrLn $ "δt/dz = " ++ showF 2 (δt pars0 / dz pars0)
  putStrLn $ "h/(u·dt) = " ++ showF 2 (h pars0 / (u pars0 * dt pars0))

ppars :: (Double, Double, Double) → Pars
ppars (α,kk,dd) = Pars{..}
  where
    method = Explicit

    ee = ee' + rr * ttm * log (kk / kk')
      where kk' = 1.6e6
            ee' = 8e4

    dz = δw * 0.9
    dt = dz/u * 0.001
    ii = round (3e-2 / dz)
    nn = round $ h/(u*dt) * (bool α 5 (α == 3)) / (bool 1 1.2 (α == 1))

    h = fromIntegral ii * dz

    ww = \xx tt → kk * xx**α * exp (-ee / (rr*tt))
    r_xx = dd * dt/dz**2
    r_tt = λ/(ρ*cc) * dt/dz**2

    u = sqrt $ 2*kk*λ / (qq*ρ*(ttm - tt0)) * (rr*ttm**2/ee)**2 * exp(-ee/(rr*ttm))
    δt = λ / (ρ*cc*u)
    β = rr*ttm/ee
    δw = δt*β
