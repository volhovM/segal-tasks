module Main where
import           Control.Monad                          (forM_, void, when)
import           Control.Monad.IO.Class                 (liftIO)
import qualified Data.Vector                            as V
import           Graphics.Rendering.Chart.Backend.Cairo (toFile)
import           Graphics.Rendering.Chart.Easy          (EC, Layout, black, def, execEC,
                                                         laxis_generate, layout_title,
                                                         layout_x_axis, liftEC, line, line_color,
                                                         line_width, opaque, plot, plot_lines_style,
                                                         plot_lines_values, red, scaledAxis,
                                                         setColors, withOpacity, (.=))
import           Graphics.Rendering.Chart.Gtk           (updateCanvas)
import           Graphics.Rendering.Chart.Renderable    (toRenderable)

import           Graphics.UI.Gtk                        hiding (Layout)
import           Numeric                                (showFFloat)

import           Solve                                  (Method (..), Pars (..), solve)

(//) :: Int -> Int -> Double
n // m = fromIntegral n / fromIntegral m

fade :: [Double]
fade = map (max 0) $ [0.3,0.28..]

graph :: Int -> Pars -> Method -> EC (Layout Double Double) ()
graph it pars method = do
    layout_title .= show method
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
        take it $
        map (zip [0, dx pars ..] . V.toList) $
        solve method pars

genPars :: Double -> Double -> Int -> Bool -> Pars
genPars s r n constant =
  let
    ((a0,b0,c0),(a1,b1,c1)) =
        if constant
        then ((1,0,1), (1,0,0))
        else ((1,0,0), (1,0,0))
    i = if constant then 0 else n `div` 2
  in Pars
  { s=s, r=r
  , dx=1//n
  , v0 = V.fromList $ replicate i 0 ++ [1] ++ replicate (n - i) 0
  , a0=a0, b0=b0, c0=c0
  , a1=a1, b1=b1, c1=c1
  }

main :: IO ()
main = do
    initGUI
    window <- windowNew

    onDestroy window mainQuit

    let itMAX = 100

    vboxpanel <- vBoxNew False 5

    adj_s       <- adjustmentNew 0 0 2 0.01 0.01 0
    scroller_s  <- hScaleNew adj_s
    set scroller_s [scaleDigits := 2]

    adj_r       <- adjustmentNew 0 0 1 0.01 0.01 0
    scroller_r  <- hScaleNew adj_r
    set scroller_r [scaleDigits := 2]

    -- adj_dt       <- adjustmentNew 0.001 0.001 0.1 0.001 0.001 0
    -- scroller_dt  <- hScaleNew adj_dt
    -- set scroller_dt [scaleDigits := 3]

    adj_n       <- adjustmentNew 20 2 100 1 2 1
    scroller_n  <- hScaleNew adj_n
    set scroller_n [scaleDigits := 0]

    adj_it      <- adjustmentNew 10 1 itMAX 1 1 0
    scroller_it <- hScaleNew adj_it
    set scroller_it [scaleDigits := 0]

    toggleCnst <- toggleButtonNewWithLabel "constant"
    do
      hbox <- hBoxNew False 5
      boxPackStart hbox toggleCnst PackNatural 0
      boxPackStart vboxpanel hbox PackNatural 0

    let withLabel l x = do
          hbox <- hBoxNew False 5
          label <- labelNew (Just l)
          boxPackStart hbox label PackNatural 0
          boxPackStart hbox x PackGrow 0
          boxPackStart vboxpanel hbox PackNatural 0
    let addToPanelLabel x =
          labelNew (Just x) >>= \l ->
          boxPackStart vboxpanel l PackNatural 0 >>
          return l
    --boxPackStart vboxpanel toggleSoft PackNatural 0
    withLabel "s" scroller_s
    withLabel "r" scroller_r
    labelSR <- addToPanelLabel "s+2r"
    -- withLabel "Δt" scroller_dt

    withLabel "n" scroller_n

    -- labelU <- addToPanelLabel "u"
    -- labelK <- addToPanelLabel "κ"
    labelDX <- addToPanelLabel "Δx"

    withLabel "t" scroller_it

    area1 <- drawingAreaNew
    area2 <- drawingAreaNew
    area3 <- drawingAreaNew
    area4 <- drawingAreaNew
    area5 <- drawingAreaNew
    table <- tableNew 2 3 True
    tableSetRowSpacings table 5
    tableSetColSpacings table 5
    tableAttachDefaults table vboxpanel 0 1 0 1
    tableAttachDefaults table area1 0 1 1 2
    tableAttachDefaults table area2 1 2 0 1
    tableAttachDefaults table area3 1 2 1 2
    tableAttachDefaults table area4 2 3 0 1
    tableAttachDefaults table area5 2 3 1 2

    let
      showPic :: EC (Layout Double Double) () -> DrawingArea -> IO ()
      showPic gr ar = void $ updateCanvas (toRenderable (execEC gr)) ar

      refreshPic = do
        s  <- adjustmentGetValue adj_s
        r  <- adjustmentGetValue adj_r
        --dt <- adjustmentGetValue adj_dt
        n  <- adjustmentGetValue adj_n
        it <- adjustmentGetValue adj_it
        constant <- toggleButtonGetActive toggleCnst
        let dx = 1 / n
        -- let u = s * dx / dt
            -- kp = r * dx**2 / dt
        -- labelSetLabel labelU $ "u = " ++ showFFloat (Just 4) u ""
        -- labelSetLabel labelK $ "κ = " ++ showFFloat (Just 4) kp ""
        labelSetLabel labelSR $ "s+2r = " ++ showFFloat (Just 2) (s+2*r) ""
        labelSetLabel labelDX $ "Δx = " ++ showFFloat (Just 5) dx ""
        let pars = genPars s r (round n) constant
        showPic (graph (round it) pars Leapfrog) area1
        showPic (graph (round it) pars ExplicitUpstream) area2
        showPic (graph (round it) pars ExplicitDownstream) area3
        --showPic (graph pars ImplicitUpstream) area4
        --showPic (graph pars ImplicitDownstream) area5

    onValueChanged adj_s  $ refreshPic
    onValueChanged adj_r $ refreshPic
    onValueChanged adj_n  $ refreshPic
    onValueChanged adj_it $ refreshPic
    onToggled toggleCnst $ refreshPic

    window `on` configureEvent $ do
      (w, h) <- eventSize
      liftIO . putStrLn $ "Resizing: " ++ show w ++ " " ++ show h
      liftIO $ refreshPic
      return False

    timeoutAdd (const True <$> refreshPic) 100

    set window
        [ containerChild := table
        , containerBorderWidth := 10
        , windowTitle := "fdm"
        ]

    widgetShowAll window
    mainGUI
