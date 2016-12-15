{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import           Numeric (showEFloat, showFFloat)

(<×>) :: Applicative f ⇒ f a → f b → f (a, b)
x <×> y = (,) <$> x <*> y

data Method = Explicit | Implicit Int

instance Show Method where
  show Explicit     = "explicit"
  show (Implicit n) = "implicit " ++ show n

data Pars = Pars
  -- method parameters
  { method :: Method
  , dz     :: Double  -- height step
  , ii     :: Int     -- number of height steps
  , dt     :: Double  -- time step
  , nn     :: Int     -- number of time steps

  -- equations parameters
  , α      :: Double
  , kk     :: Double
  , dd     :: Double
  , ee     :: Double

  -- calculatable
  , ww     :: Double → Double → Double  -- reaction speed from X and T
  , r_xx   :: Double  -- reynolds number for X
  , r_tt   :: Double  -- reynolds number for T
  , h      :: Double  -- height

  -- for tuning steps
  , u      :: Double
  , δt     :: Double
  , δw     :: Double
  }


dd0 = λ/(ρ*cc)
ttm = tt0 + qq/cc
rr = 8.314
qq = 7e5
tt0 = 293
cc = 1980
ρ = 830
λ = 0.13

showF, showE :: RealFloat a ⇒ Int → a → String
showF k x = showFFloat (Just k) x ""
showE k x = showEFloat (Just k) x ""


type Pars' = (Int, Double, Int, Double, Int, Double, Double, Double, Double)

rpars :: Pars' → Pars
rpars (nmethod,dz,ii,dt,nn,α,kk,dd,ee) = Pars{..}
  where
    method = if nmethod == -1 then Explicit else Implicit nmethod
    h = fromIntegral ii * dz

    ww = \xx tt → kk * xx**α * exp (-ee / (rr*tt))
    r_xx = dd * dt/dz**2
    r_tt = λ/(ρ*cc) * dt/dz**2

    u = sqrt $ 2*kk*λ / (qq*ρ*(ttm - tt0)) * (rr*ttm**2/ee)**2 * exp(-ee/(rr*ttm))
    δt = λ / (ρ*cc*u)
    β = rr*ttm/ee
    δw = δt*β

wpars :: Pars → Pars'
wpars Pars{..} = (case method of {Explicit → (-1); Implicit n → n},dz,ii,dt,nn,α,kk,dd,ee)

αVals = [0.95, 1.0, 2.0, 3.0]
kkVals = [1.6e6, 5e6, 1e7, 1e8]
ddVals = [8e-12, 1e-11, 1e-10, 1e-9, 1e-8, 7.91e-8, 9e-8]

--αVals = [2.0]
--kkVals = [1e8]
--ddVals = [8e-12, 1e-11, 1e-10]

pdir :: (Double, Double, Double) → FilePath
pdir ps = "data/" ++ show ps
