module Main where

import           Control.Monad                          (forM_)
import qualified Data.Vector                            as V
import           Graphics.Rendering.Chart.Backend.Cairo (toFile)
import           Graphics.Rendering.Chart.Easy          (EC, Layout, black, def,
                                                         laxis_generate, layout_x_axis,
                                                         liftEC, line, line_color,
                                                         line_width, opaque, plot,
                                                         plot_lines_style,
                                                         plot_lines_values, red,
                                                         scaledAxis, setColors,
                                                         withOpacity, (.=))

import           Graphics.UI.Gtk                        (initGUI, mainGUI, widgetShowAll,
                                                         windowNew)
import           Solve                                  (Method (..), Pars (..), solve)

(//) :: Int -> Int -> Double
n // m = fromIntegral n / fromIntegral m

myPars :: Pars
myPars = Pars
  { u=0.01, kappa=0.0003
  , dx=1//(V.length (v0 myPars) - 1), dt=1
  , v0 = V.fromList $ replicate 10 0 ++ [1] ++ replicate 10 0
  , a0=1, b0=0, c0=0
  , a1=1, b1=0, c1=0
  }

fade :: [Double]
fade = map (max 0) $ [0.5,0.48..]

graph :: EC (Layout Double Double) ()
graph = do
    layout_x_axis . laxis_generate .= scaledAxis def (0,1)

    forM_ (zip fade $ tail ppoints) $ \(op, pts) -> do
      setColors [black `withOpacity` op]
      plot (line "" [pts])
    -- setColors [opaque red]
    -- plot (line "" [head ppoints])
    plot $ liftEC $ do
        plot_lines_values .= [head ppoints]
        plot_lines_style . line_width .= 2
        plot_lines_style . line_color .= opaque red
  where
    ppoints :: [[(Double, Double)]]
    ppoints =
        reverse $
        take 23 $
        map (zip [0, dx myPars ..] . V.toList) $
        solve ExplicitUpstream myPars

mm :: IO ()
--mm = toFile (def {_fo_size = (400, 350)})  "some.png" graph
mm = toFile def "some.png" graph

main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  widgetShowAll window
  mainGUI
