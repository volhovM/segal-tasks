module Main where

import           Solve

import           Control.Monad                          (void)
import           Graphics.EasyPlot                      hiding (plot)
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy          hiding ((:>))

-- graph of the x coordinate
graph :: Int → Method → Pars → EC (Layout Double Double) ()
graph steps method pars = plot (line "" [ppoints])
  where
    ppoints = take steps $ map (fmap getx) $ solve method pars
    getx (x,y,z) = x

---- drawing some simple graphs
myPars :: Pars
myPars = Pars
  { r = 28
  , δt = 0.005
  , v₀ = (1,1,1)
  }

-- interactive 3d graph
trid :: Int → Method → IO ()
trid n method = void $ plot' [Interactive, Debug] X11 $ Data3D [Color Magenta] [] (map snd $ take n $ solve method myPars)

makeGraphs :: IO ()
makeGraphs = do
  toFile def "1.explicit-euler.png" (graph 10000 ExplicitEuler myPars)
  toFile def "2.implicit-euler.png" (graph 10000 ImplicitEuler myPars)
  toFile def "3.runge-kutta.png" (graph 10000 RungeKutta myPars)
  toFile def "4.adams.png" (graph 10000 Adams myPars)

main :: IO ()
main = makeGraphs
