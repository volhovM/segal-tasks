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

import qualified Tridiagonal                      (solve)

data Method = Explicit | Implicit

instance Show Method where
  show Explicit = "explicit"
  show Implicit = "implicit"

-- X, T
type Val = (VU.Vector Double, VU.Vector Double)

-- t, X, T, W
type Val' = (Double, VU.Vector Double, VU.Vector Double, VU.Vector Double)

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

implicitStep :: Pars → Val → Val
implicitStep Pars{..} val@(xxn,ttn) =
    --predictCorrect step val
    iterateN 1 step val
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

rarefy :: VG.Vector v a ⇒ Int → v a → v a
rarefy k v =
  let n = VG.length v
      r = n `div` k
  in VG.generate k $ \i → v!(i*r)

vcount :: VU.Unbox a ⇒ (a → Bool) → VU.Vector a → Int
vcount p v = VU.sum $ flip VU.map v $ \x → bool 0 1 (p x)

runMethod :: Method → Pars → Int → Int → Int → IO (V.Vector Val')
runMethod method pars@Pars{ww, dt} hs n k =
    flip evalStateT (initial pars)
    $ V.generateM n (\i → vnext' i)
  where
    vnext = case method of
      Explicit → explicitStep
      Implicit → implicitStep

    vnext' :: Int → StateT Val IO Val'
    vnext' i = do
      cur@(xx,tt) <- get
      let t = fromIntegral (i * k) * dt
          xx' = rarefy hs xx
          tt' = rarefy hs tt
          ww' = VU.zipWith ww xx' tt'
      let topr = intercalate "   "
                 [ show i ++ "/" ++ show n
                 --, "X = " ++ show (xx'!1)
                 --, "T = " ++ show (tt'!1)
                 --, "w = " ++ show (w'!1)
                 , "X = " ++ show (VU.last xx')
                 , "T = " ++ show (VU.last tt')
                 , "W = " ++ show (VU.last ww')
                 --, "nanX " ++ show (vcount isNaN xx)
                 --, "infX " ++ show (vcount isInfinite xx)
                 --, "nanT " ++ show (vcount isNaN tt)
                 --, "infT " ++ show (vcount isInfinite tt)
                 ]
      --let next@(xx,tt) = iterateN k vnext cur
      let next = iterateN k (vnext pars) cur
      when (i `mod` 10 == 0) $ liftIO $ putStrLn topr
      put next
      pure (t, xx', tt', ww')

initial :: Pars → Val
initial Pars{ii} =
    ( VU.generate (ii+1) $ \i → if i == 0 then 0 else 1
    , VU.generate (ii+1) $ \i → if i == 0 then ttm else tt0
    )


data Pars = Pars
  -- method parameters
  { dz   :: Double  -- height step
  , ii   :: Int     -- number of height steps
  , dt   :: Double  -- time step
  , nn   :: Int     -- number of time steps

  -- equations parameters
  , α    :: Double
  , kk   :: Double
  , dd   :: Double
  , ee   :: Double

  -- calculatable
  , ww   :: Double → Double → Double  -- reaction speed from X and T
  , r_xx :: Double  -- reynolds number for X
  , r_tt :: Double  -- reynolds number for T
  , h    :: Double  -- height
  }


pars1 :: Pars
pars1 = Pars{..}
  where
    dz = δw * 0.5
    --dz = δw * 0.1
    --dz = δw * 0.05
    --ii = round (δt * 10 / dz)
    ii = round (δt * 15 / dz)
    --ii = round (δt * 20 / dz)
    --ii = round (δt * 30 / dz)
    --dt = dz/u * 0.001
    dt = dz/u * 0.005
    --dt = dz/u * 0.01
    --dt = dz/u * 0.05
    --dt = dz/u * 0.1
    nn = round $ h/(u*dt) / 2
    --nn = round $ h/(u*dt) / 1.5
    --nn = round $ h/(u*dt)
    --nn = round $ h/(u*dt) * 2
    --nn = round $ h/(u*dt) * 5

    α  = 1
    --α  = 0.5
    kk = 1.6e6
    dd = 8e-12
    --dd = λ / (ρ*cc)
    ee = 8e4
    --ee = 1e5

    --kk = kk' * exp ((ee - ee') / (rr*ttm))
    --  where kk' = 1.6e6
    --        ee' = 8e4


    ww = \xx tt → kk * xx**α * exp (-ee / (rr*tt))
    r_xx = dd * dt/dz**2
    r_tt = λ/(ρ*cc) * dt/dz**2

    h = fromIntegral (ii-1) * dz
    u = sqrt $ 2*kk*λ / (qq*ρ*(ttm - tt0)) * (rr*ttm**2/ee)**2 * exp(-ee/(rr*ttm))
    δt = λ / (ρ*cc*u)
    β = rr*ttm/ee
    δw = δt*β

pars0 = pars1

--ii = round (δt * 4 / dz)
--ii = round (δt * 15 / dz)
--ii = round $ δt * 30 / dz


--dt = dz/u * 0.003
--dt = dz/u * 0.005
--dt = dz/u * 0.01
--dt = dz/u * 0.1

--nn = round $ h/(u*dt) / 1.5
--nn = round $ h/(u*dt)
--nn = round $ h/(u*dt) * 2

dd0 = λ/(ρ*cc)

ttm = tt0 + qq/cc

rr = 8.314
qq = 7e5
tt0 = 293
cc = 1980
ρ = 830
λ = 0.13
