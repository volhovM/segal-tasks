module Main where

import           Solve

import           Control.Monad                          (void)
import           Graphics.EasyPlot                      hiding (plot)
import           Graphics.Rendering.Chart.Backend.Cairo hiding (Color)
import           Graphics.Rendering.Chart.Easy          (EC, def, execEC, line,
                                                         plot)
import           Graphics.Rendering.Chart.Gtk           (updateCanvas)
import           Graphics.Rendering.Chart.Renderable    (toRenderable)
import           Graphics.UI.Gtk                        hiding (Color)

import           Control.Monad.IO.Class                 (liftIO)


-- graph of the x coordinate
--graph :: Int → Method → Pars → EC (Layout Double Double) ()
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

--currentPlot :: Method → Pars → EC (Layout Double Double) ()
currentPlot m p = return $ graph 5000 m p

main :: IO ()
main = do
  initGUI

  window      ← windowNew
  vbox        ← vBoxNew False 5
  hboxpanel   ← hBoxNew False 5
  button1     ← buttonNewWithLabel "SNCE"
  button2     ← buttonNewWithLabel "VISU"
  button3     ← buttonNewWithLabel "Generate BIFU"
  button4     ← buttonNewWithLabel "Generate NEWT"
  adj_r       ← adjustmentNew 0.0 0.0 4.0 0.01 1.0 0.0
  adj_x₀      ← adjustmentNew 0.0 0.0 1.0 0.01 1.0 0.0
  scroller_r  ← hScaleNew adj_r
  scroller_x₀ ← hScaleNew adj_x₀

  -- quit button doesn't work :(quitButton ← buttonNewWithLabel "Quit"
  panellabel ← labelNew $ Just "Current: SNCE"
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
    refreshPic quick = do
      l ← labelGetLabel panellabel
      if l == "Current: BIFU" && quick
      then return ()
      else do
        r  ← adjustmentGetValue adj_r
        δt ← adjustmentGetValue adj_x₀
        drawPic . getPic =<< currentPlot Adams (Pars r δt (0,0,0))
        -- TODO: add v₀ parameter
        return ()
{-
    dumpToFile !name !dtype !label = do
      labelSetLabel panellabel label
      putStrLn $ "Calculating..., state " ++ label
      let !renderable = getPic $! dtype
      putStrLn "To file..."
      --renderableToFile (FileOptions (1200, 600) PNG) name renderable
      putStrLn "Drawing on screen..."
      --drawPic renderable
      putStrLn "Done"
      return $! ()
-}


  onValueChanged adj_r  $ refreshPic False
  onValueChanged adj_x₀ $ refreshPic False

  onClicked button1 $ labelSetLabel panellabel "Current: SNCE" >> refreshPic True
  onClicked button2 $ labelSetLabel panellabel "Current: VISU" >> refreshPic True

  window `on` configureEvent $ do
    (w, h) ← eventSize
    liftIO . putStrLn $ "Resizing: " ++ show w ++ " " ++ show h
    liftIO $ refreshPic True
    return False

  widgetShowAll window
  mainGUI
