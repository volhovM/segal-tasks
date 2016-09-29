module Solve
  ( Method(..)
  , Pars(..)
  , solve
  ) where

---- vector stuff

type Vec = (Double, Double, Double)

infixl 6 .+., .-.
(.+.), (.-.) :: Vec → Vec → Vec
(x1,y1,z1) .+. (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
(x1,y1,z1) .-. (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

infix 7 *.
(*.) :: Double → Vec → Vec
s *. (x, y, z) = (s*x, s*y, s*z)

infix 7 ./
(./) :: Vec → Double → Vec
(x, y, z) ./ s = (x/s, y/s, z/s)


---- solver

data Pars = Pars { r, δt :: Double
                 , v₀ :: Vec
                 }

data Method = ImplicitEuler | ExplicitEuler | RungeKutta | Adams

-- _, _ ↦ [(t₀,v₀), (t₁,v₁), (t₂,v₂), …]
solve :: Method → Pars → [(Double, Vec)]
solve method Pars{..} = zip [0,δt..] vecs where
  -- [v₀, v₁, v₂, …]
  vecs :: [Vec]
  vecs = vall [v₀]

  -- [vᵢ, vᵢ₋₁, vᵢ₋₂, …, v₀] ↦ [vᵢ, vᵢ₊₁, vᵢ₊₂, …]
  vall :: [Vec] → [Vec]
  vall vs = head vs : vall (vnext vs : vs)

  vnext :: [Vec] → Vec
  vnext = case method of
    ExplicitEuler → explicitEuler
    ImplicitEuler → implicitEuler
    RungeKutta    → rungeKutta
    Adams         → adams

  -- v'(t) = f(v(t))
  f :: Vec → Vec
  f (x, y, z) =
      ( σ*y - σ*x
      , r*x - x*z - y
      , x*y - b*z
      ) where σ = 10.0; b = 8.0/3.0

  predictCorrect :: (Vec → Vec) → Vec → Vec
  predictCorrect foo v = foo (foo v)

  explicitEuler :: [Vec] → Vec
  explicitEuler (vᵢ:_) = vᵢ .+. δt *. f vᵢ

  implicitEuler :: [Vec] → Vec
  implicitEuler (vᵢ:_) = predictCorrect step vᵢ
    where step v = vᵢ .+. (δt / 2) *. (f vᵢ .+. f v)

  rungeKutta :: [Vec] → Vec
  rungeKutta (vᵢ:_) = vᵢ .+. δt *. k
    where
      k  = (k₀ .+. 2*.k₁ .+. 2*.k₂ .+. k₃) ./ 6
      k₀ = f vᵢ
      k₁ = f (vᵢ .+. (δt / 2) *. k₀)
      k₂ = f (vᵢ .+. (δt / 2) *. k₁)
      k₃ = f (vᵢ .+. δt *. k₂)

  adams :: [Vec] → Vec
  adams (vi:vi1:vi2:_) = predictCorrect step vi
    where step v = vi .+. (δt / 24) *. (9 *. f v .+. 19 *. f vi .-. 5 *. f vi1 .+. f vi2)
  adams vs = rungeKutta vs -- fall back to Runge-Kutta for first steps
