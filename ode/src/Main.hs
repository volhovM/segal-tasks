module Main where

import           Solve

import           Control.Monad                          (void)
import           Graphics.Rendering.Chart.Backend.Cairo hiding (Color)
import           Graphics.Rendering.Chart.Easy          (EC, def, execEC, line,
                                                         plot)
import           Graphics.Rendering.Chart.Gtk           (updateCanvas)
import           Graphics.Rendering.Chart.Renderable    (toRenderable)
import           Graphics.UI.Gtk                        hiding (Color)

import           Control.Monad.IO.Class                 (liftIO)

import           System.Posix.Process                   (forkProcess)

import           EasyPlot                               hiding (plot)

-- graph of the x coordinate
--graph :: Int → Method → Pars → EC (Layout Double Double) ()
graph steps method pars = plot (line "" [ppoints])
  where
    ppoints = take steps $ map (fmap getx) $ solve method pars
    getx (x,y,z) = x

-- interactive 3d graph
trid :: Method → Pars → IO ()
trid method p = void $ plot' [Interactive, Debug] X11 $ Data3D [Color Magenta] [] (map snd $ take n $ solve method p)
  where n = 5000

  {-
makeGraphs :: IO ()
makeGraphs = do
  toFile def "1.explicit-euler.png" (graph 10000 ExplicitEuler myPars)
  toFile def "2.implicit-euler.png" (graph 10000 ImplicitEuler myPars)
  toFile def "3.runge-kutta.png" (graph 10000 RungeKutta myPars)
  toFile def "4.adams.png" (graph 10000 Adams myPars)
-}

--currentPlot :: Method → Pars → EC (Layout Double Double) ()
currentPlot m p = return $ graph 5000 m p

main :: IO ()
main = do
  initGUI

  window      ← windowNew
  vbox        ← vBoxNew False 5
  hboxpanel   ← hBoxNew False 5
  button1     ← buttonNewWithLabel "Explicit euler"
  button2     ← buttonNewWithLabel "Implicit euler"
  button3     ← buttonNewWithLabel "Runge-Kutta"
  button4     ← buttonNewWithLabel "Adams"
  buttonTrid  ← buttonNewWithLabel "3d"
  adj_r       ← adjustmentNew 0.0 0.0 30.0 0.1 1.0 0.0
  adj_x₀      ← adjustmentNew 0.0 0.0 0.1 0.001 1.0 0.0
  scroller_r  ← hScaleNew adj_r
  scroller_x₀ ← hScaleNew adj_x₀

  -- quit button doesn't work :(quitButton ← buttonNewWithLabel "Quit"
  panellabel ← labelNew $ Just (show ExplicitEuler)
  delim1 ← hSeparatorNew

  onDestroy window mainQuit
  window `on` focus $ \directiont →
    putStrLn ("Focused" ++ show directiont) >> return False
  set window [
    windowDefaultWidth := 800,
    windowDefaultHeight := 600,
    containerChild := vbox,
    containerBorderWidth := 10,
    windowTitle := "HW1"]

  boxPackStart vbox panellabel PackNatural 0
  miscSetAlignment panellabel 0 0
  boxPackStart vbox hboxpanel PackNatural 0

  boxPackStart hboxpanel button1 PackNatural 0
  boxPackStart hboxpanel button2 PackNatural 0
  boxPackStart hboxpanel button3 PackNatural 0
  boxPackStart hboxpanel button4 PackNatural 0
  boxPackStart hboxpanel buttonTrid PackNatural 0

  set scroller_r  [scaleDigits := 2]
  set scroller_x₀ [scaleDigits := 2]
  boxPackStart hboxpanel scroller_r PackGrow 0
  boxPackStart hboxpanel scroller_x₀ PackGrow 0

  boxPackStart vbox delim1 PackNatural 5

  area ← drawingAreaNew
  boxPackStart vbox area PackGrow 20

  let
    getPic = toRenderable . execEC
    drawPic x = updateCanvas x area >> return ()
    refreshPic = do
      l ← labelGetLabel panellabel
      r  ← adjustmentGetValue adj_r
      δt ← adjustmentGetValue adj_x₀
      drawPic . getPic =<< currentPlot (read l) (Pars r δt (1,1,1))

  onValueChanged adj_r  $ refreshPic
  onValueChanged adj_x₀ $ refreshPic

  onClicked button1 $ labelSetLabel panellabel (show ExplicitEuler) >> refreshPic
  onClicked button2 $ labelSetLabel panellabel (show ImplicitEuler) >> refreshPic
  onClicked button3 $ labelSetLabel panellabel (show RungeKutta) >> refreshPic
  onClicked button4 $ labelSetLabel panellabel (show Adams) >> refreshPic
  onClicked buttonTrid $ do
    l ← labelGetLabel panellabel
    r  ← adjustmentGetValue adj_r
    δt ← adjustmentGetValue adj_x₀
    void $ forkProcess $ trid (read l) (Pars r δt (1,1,1))

  window `on` configureEvent $ do
    (w, h) ← eventSize
    liftIO . putStrLn $ "Resizing: " ++ show w ++ " " ++ show h
    liftIO $ refreshPic
    return False

  timeoutAdd (const True <$> refreshPic) 100

  widgetShowAll window
  mainGUI

