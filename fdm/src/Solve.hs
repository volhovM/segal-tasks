module Solve where

import           Data.Vector ((!))
import qualified Data.Vector as V

import qualified Tridiagonal (solve)

data Method = ExplicitUpstream
            | ExplicitDownstream
            | Leapfrog
            | ImplicitUpstream
            | ImplicitDownstream
            deriving (Eq)

instance Show Method where
  show ExplicitUpstream   = "явная против потока"
  show ExplicitDownstream = "явная по потоку"
  show Leapfrog           = "чехарда"
  show ImplicitUpstream   = "неявная против потока"
  show ImplicitDownstream = "неявная по потоку"

{-

function T = T(x,t), x∈[0,1], t∈[0,+∞)

- pde
  ∂T/∂t + u ∂T/∂x − κ ∂²T/∂x² = 0
- initial condition
  T(x,0) = T₀(x)
- boundary conditions
  1. a₀·T(0,t) + b₀·∂T/∂x(0,t) = c₀
  2. a₁·T(1,t) + b₁·∂T/∂x(1,t) = c₁

s = uΔt/Δx
r = κΔt/Δx²

-}

data Pars = Pars
    { u, kappa               :: Double
    , dx, dt                 :: Double
    , v0                     :: V.Vector Double
    , a0, b0, c0, a1, b1, c1 :: Double
    }

solve :: Method -> Pars -> [V.Vector Double]
solve method Pars{..} = vall [v0]
  where
    kk :: Int
    kk = V.length v0 - 1

    s, r :: Double
    s = u*dt / dx
    r = kappa*dt / dx**2

    -- [vₙ, vₙ₋₁, vₙ₋₂, …, v₀] ↦ [vₙ, vₙ₊₁, vₙ₊₂, …]
    vall :: [V.Vector Double] -> [V.Vector Double]
    vall vs = head vs : vall (vnext method vs : vs)

    -- [vₙ, vₙ₋₁, vₙ₋₂, …, v₀] ↦ vₙ₊₁
    vnext :: Method -> [V.Vector Double] -> V.Vector Double
    vnext _                  []         = error "impossible"
    vnext ExplicitUpstream   (vn:_)     = explicitly $ \k -> (s+r) * vn!(k-1) + (1-s-2*r) * vn!k + r * vn!(k+1)
    vnext ExplicitDownstream (vn:_)     = explicitly $ \k -> r * vn!(k-1) + (1+s-2*r) * vn!k + (-s+r) * vn!(k+1)
    vnext Leapfrog           (vn:vn1:_) = explicitly $ \k -> vn1!k + (s+2*r) * vn!(k-1) + (-4*r) * vn!k + (-s+2*r) * vn!(k+1)
    vnext Leapfrog           vs         = vnext ExplicitUpstream vs -- fall back for the first step
    vnext ImplicitUpstream   (vn:_)     = implicitly $ \k -> (-s-r, 1+s+2*r, -r, vn!k)
    vnext ImplicitDownstream (vn:_)     = implicitly $ \k -> (-r, 1-s+2*r, s-r, vn!k)

    explicitly :: (Int -> Double) -> V.Vector Double
    explicitly f =
        V.generate (kk+1) $ \k ->
        if | k == 0  -> (c0 - b0/dx * f 1) / (a0 - b0/dx)
           | k == kk -> (c1 + b1/dx * f (kk-1)) / (a1 + b1/dx)
           | True    -> f k

    implicitly :: (Int -> (Double, Double, Double, Double)) -> V.Vector Double
    implicitly f =
        Tridiagonal.solve (kk+1) $ \k ->
        if | k == 0  -> (error "no first a", a0-b0/dx, b0/dx, c0)
           | k == kk -> (a1+b1/dx, -b1/dx, error "no last c", c1)
           | True    -> f k
