{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Control.DeepSeq          (force)
import           Control.Lens             (view, _1, _2, _3, _4)
import           Control.Monad            (forM, forM_, join, void, when)
import           Control.Monad.Extra      (whenJustM)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ReaderT (..), ask)
import           Control.Monad.Trans      (lift)
import           Data.Bifunctor           (bimap, second)
import           Data.Bool                (bool)
import qualified Data.ByteString          as B
import           Data.IORef               (IORef, modifyIORef, newIORef, readIORef,
                                           writeIORef)
import           Data.Ord                 (comparing)
import           Data.Serialize           (decode)
import qualified Data.Vector              as V
import           Data.Vector.Generic      ((!))
import qualified Data.Vector.Generic      as VG
import           Data.Vector.Serialize    ()
import qualified Data.Vector.Unboxed      as VU
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk          as G
import qualified Graphics.UI.Gtk.Gdk.GC   as G
import           System.Environment       (getArgs)
import           System.Exit              (exitSuccess)

import           Types

(//) :: Int → Int → Double
n // m = fromIntegral n / fromIntegral m

(<&>) :: Functor f ⇒ f a → (a → b) → f b
(<&>) = flip (<$>)


type Transformation = (Double → Double, Double → Double)
type RenderS a = ReaderT Transformation C.Render a

transformation :: ((Double, Double), (Double, Double)) → (Double, Double) → Transformation
transformation (xb,yb) (width,height) = (tx, ty)
  where
    withLabels = height >= 400

    xshift1 = width * 0.1
    xshift2 = width * 0.1
    yshift1 = height * 0.035
    yshift2 = height * (bool 0.01 0.05 withLabels)

    dxb = (0+xshift1, width - xshift2)
    dyb = (height - yshift1, 0+yshift2)

    t (a1,a2) (b1,b2) x = α*x + β
      where
        α = (b1 - b2) / (a1 - a2)
        β = b1 + a1 * (b2 - b1) / (a1 - a2)


    tx x = t xb dxb x
    ty y = t yb dyb y

walkLineT :: Transformation → VU.Vector (Double, Double) → C.Render ()
walkLineT (tx,ty) points = do
  C.moveTo (tx $ fst $ points!0) (ty $ snd $ points!0)
  forM_ [1..VG.length points-1] $ \i → do
    C.lineTo (tx $ fst $ points!i) (ty $ snd $ points!i)

traces :: String → ([Double], (Double, Double)) → (Double, Double) → Int → (Int → VU.Vector Double) → C.Render ()
traces label (xbounds, yb) (width, height) nvals vals = do
  let withLabels = height >= 400

  let xb@(xb1, xb2) = (head xbounds, last xbounds)
  let (yb1, yb2) = yb
  let tr@(tx,ty) = transformation (xb, yb) (width, height)
  --let tred = transformed (xb, yb) (width, height)

  -- background
  C.setSourceRGB 1 1 1
  C.paint

  -- traces
  C.setLineWidth 1
  C.setSourceRGBA (255/255) (0/255) (140/255) 0.1
  forM_ [0..nvals-1] $ \i → do
    let vv = vals i
    walkLineT tr (VG.zip vv (VG.fromList [0..fromIntegral (VG.length vv - 1)]))
    C.stroke

  let
    aline :: (Double, Double) → (Double, Double) → C.Render ()
    aline (x1, y1) (x2, y2) = do
      C.moveTo x1 y1
      C.lineTo x2 y2
      C.stroke

  when withLabels $ do
    -- label
    C.setFontSize 24
    C.setSourceRGBA 0 0 0 1
    C.moveTo 10 25
    C.showText label

    -- axes
    C.setLineWidth 1
    C.setSourceRGBA 0 0 0 0.5
    C.setFontSize 10
    aline (tx (fst xb), height-15) (tx (snd xb), height-15)
    forM_ xbounds $ \t → do
      aline (tx t, height-20) (tx t, height-10)
      C.moveTo (tx t) (height-3)
      C.showText $
        if | t == 0  → "0"
           | t < 0.1 → showE 1 t
           | t < 10  → showF 1 t
           | True    → showF 0 t


current :: VU.Vector Double → ([Double], (Double, Double)) → (Double, Double) → C.Render ()
current vals1 (xbounds, yb) (width, height) = do
  let xb = (head xbounds, last xbounds)
  --let tred = transformed (xb, yb) (width, height)
  let tr@(tx,ty) = transformation (xb, yb) (width, height)

  walkLineT tr (VG.zip vals1 (VG.fromList [0..fromIntegral (VG.length vals1 - 1)]))
  C.setLineWidth 2
  C.setSourceRGBA (0/255) (130/255) (97/255) 1
  C.stroke


withoutFlickering :: G.DrawingArea → IO () → IO ()
withoutFlickering area action = do
  drawWindow ← G.widgetGetDrawWindow area
  (width, height) <- G.widgetGetSize area
  region <- G.regionRectangle $ G.Rectangle 0 0 width height
  G.drawWindowBeginPaintRegion drawWindow region
  action
  G.drawWindowEndPaint drawWindow


badd :: (G.BoxClass box, G.WidgetClass widget) ⇒ G.Packing → IO box → IO widget → IO box
badd packing mbox mwidget = do
  box ← mbox
  widget ← mwidget
  G.boxPackStart box widget packing 0
  pure box

infixl 6 ⊨, ⊭, ⊭*
(⊨), (⊭) :: (G.BoxClass box, G.WidgetClass widget) ⇒ IO box → IO widget → IO box
(⊨) = badd G.PackGrow
(⊭) = badd G.PackNatural

(⊭*) :: (G.BoxClass box, G.WidgetClass widget) ⇒ IO box → IO [widget] → IO box
(⊭*) mbox mwidgets = do
  box ← mbox
  widgets ← mwidgets
  forM_ widgets $ \w → G.boxPackStart box w G.PackNatural 0
  pure box


data ValRW m a = ValRW
  { readValRW     :: m a
  , writeValRW    :: a → m ()
  , onChangeValRW :: m () → m ()
  }

newValRW :: a → IO (ValRW IO a)
newValRW a = do
  ref ← newIORef (a, [] :: [IO ()])
  pure $ ValRW
    { readValRW = fst <$> readIORef ref
    , writeValRW = \value → do
        actions ← snd <$> readIORef ref
        writeIORef ref (value, actions)
        sequence_ actions
    , onChangeValRW = \action → do
        modifyIORef ref $ second $ (++ [action])
    }


createToggleButtonWithLabel :: ValRW IO Bool → String → IO G.ToggleButton
createToggleButtonWithLabel rv label = do
  toggleButton ← G.toggleButtonNewWithLabel label
  toggleButton `G.on` G.toggled $ do
      writeValRW rv =<< G.toggleButtonGetActive toggleButton
  pure toggleButton

data ValR m a = ValR
  { readValR     :: m a
  , onChangeValR :: m () → m ()
  }

toValR :: ValRW m a → ValR m a
toValR rwv = ValR
  { readValR = readValRW rwv
  , onChangeValR = onChangeValRW rwv
  }

instance Functor f ⇒ Functor (ValR f) where
  fmap f r = ValR
               (f <$> readValR r)
               (onChangeValR r)

instance Applicative f ⇒ Applicative (ValR f) where
  pure a  = ValR
              (pure a)
              (\_ → pure ())
  f <*> a = ValR
              (readValR f <*> readValR a)
              (\action → onChangeValR f action *> onChangeValR a action)

createLabel :: ValR IO String → IO G.Label
createLabel textR = do
  text ← readValR textR
  label ← G.labelNew (Just text)
  onChangeValR textR $ do
    text ← readValR textR
    G.labelSetText label text
  pure label

adjRW :: G.Adjustment → ValRW IO Double
adjRW adj = ValRW
  { readValRW     = G.adjustmentGetValue adj
  , writeValRW    = G.adjustmentSetValue adj
  , onChangeValRW = \action → void $ G.onValueChanged adj action
  }

adjR :: G.Adjustment → ValR IO Double
adjR adj = toValR $ adjRW adj

createRadioButtons :: ValRW IO a → [(a, String)] → IO [G.RadioButton]
createRadioButtons val ((a1,l1):names) = do
    b1 ← G.radioButtonNewWithLabel l1
    activate b1 a1
    bs ← forM names $ \(a,l) → do
      b ← G.radioButtonNewWithLabelFromWidget b1 l
      activate b a
      pure b
    pure (b1:bs)
  where
    activate b a =
      b `G.on` G.toggled $ do
        active ← G.toggleButtonGetActive b
        when active $ writeValRW val a



createPlot :: String → Int → ValR IO (Int → VU.Vector Double) → ValR IO [Double] → ValR IO Double → IO G.DrawingArea
createPlot label nvals valsVal boundsVal tVal = do
    tracesPixmapR ← newIORef =<< G.pixmapNew (Nothing :: Maybe G.Drawable) 1 1 (Just 24)
    pixmapR ← newIORef =<< G.pixmapNew (Nothing :: Maybe G.Drawable) 1 1 (Just 24)
    drawingArea ← G.drawingAreaNew

    onChangeValR tVal $ do
      t ← readValR (round <$> tVal)
      vals ← readValR valsVal
      bounds ← readValR boundsVal
      let hs = VU.length $ vals 0

      pixmap ← readIORef pixmapR
      (width, height) ← G.drawableGetSize pixmap

      -- current
      let vals1 = vals t
      let rcurrent = current vals1 (bounds,(0,fromIntegral hs-1)) (fromIntegral width, fromIntegral height)

      -- update area pixmap
      gc ← G.gcNew pixmap
      tracesp ← readIORef tracesPixmapR
      G.drawDrawable pixmap gc tracesp 0 0 0 0 (-1) (-1)
      G.renderWithDrawable pixmap rcurrent

      -- update area
      drawWindow ← G.widgetGetDrawWindow drawingArea
      G.drawDrawable drawWindow gc pixmap 0 0 0 0 (-1) (-1)

    onChangeValR valsVal $ do
      t ← readValR (round <$> tVal)
      vals ← readValR valsVal
      bounds ← readValR boundsVal

      let hs = VU.length $ vals 0

      drawWindow ← G.widgetGetDrawWindow drawingArea
      drawWindowSize ← G.drawableGetSize drawWindow

      -- update traces
      let (width, height) = drawWindowSize
      let rtraces = traces label (bounds,(0,fromIntegral hs-1)) (fromIntegral width, fromIntegral height) nvals vals
      tracesPixmap ← readIORef tracesPixmapR
      G.renderWithDrawable tracesPixmap rtraces

      -- current
      let vals1 = vals t
      let rcurrent = current vals1 (bounds,(0,fromIntegral hs-1)) (fromIntegral width, fromIntegral height)

      -- update area pixmap
      pixmap ← readIORef pixmapR
      gc ← G.gcNew pixmap
      G.drawDrawable pixmap gc tracesPixmap 0 0 0 0 (-1) (-1)
      G.renderWithDrawable pixmap rcurrent

      -- update area
      drawWindow ← G.widgetGetDrawWindow drawingArea
      G.drawDrawable drawWindow gc pixmap 0 0 0 0 (-1) (-1)
      pure ()

    drawingArea `G.on` G.exposeEvent $ (const False <$>) $ do
      G.Rectangle x y w h ← G.eventArea
      drawWindow ← G.eventWindow
      liftIO $ do
        pixmap ← readIORef pixmapR
        gc ← G.gcNew drawWindow
        drawWindowSize ← G.drawableGetSize drawWindow
        (== drawWindowSize) <$> G.drawableGetSize pixmap >>= \case
          True  → G.drawDrawable drawWindow gc pixmap x y x y w h
          False → do
            -- size changed

            t ← readValR (round <$> tVal)
            vals ← readValR valsVal
            bounds ← readValR boundsVal

            let hs = VU.length $ vals 0

            -- reallocate pixmap
            -- (leaks memory for some reason)
            pixmap ← G.pixmapNew (Just drawWindow) w h Nothing
            writeIORef pixmapR pixmap

            -- update traces
            tracesPixmap ← G.pixmapNew (Just drawWindow) w h Nothing
            writeIORef tracesPixmapR tracesPixmap
            let (width, height) = drawWindowSize
            let rtraces = traces label (bounds,(0,fromIntegral hs-1)) (fromIntegral width, fromIntegral height) nvals vals
            tracesPixmap ← readIORef tracesPixmapR
            G.renderWithDrawable tracesPixmap rtraces

            -- current
            let vals1 = vals t
            let rcurrent = current vals1 (bounds,(0,fromIntegral hs-1)) (fromIntegral width, fromIntegral height)

            -- update area pixmap
            gc ← G.gcNew pixmap
            G.drawDrawable pixmap gc tracesPixmap 0 0 0 0 (-1) (-1)
            G.renderWithDrawable pixmap rcurrent

            -- update area
            drawWindow ← G.widgetGetDrawWindow drawingArea
            G.drawDrawable drawWindow gc pixmap 0 0 0 0 (-1) (-1)
            pure ()

    pure drawingArea

-- X, T, W
type Val' = (Double, VU.Vector Double, VU.Vector Double, VU.Vector Double)

fromRightS :: Either String b → b
fromRightS (Right b) = b
fromRightS (Left a)  = error ("fromRight: " ++ a)

getvals :: FilePath → IO (VU.Vector Double, VU.Vector Double)
getvals dir =
  (,) <$> (fromRightS <$> decode <$> B.readFile (dir ++ "/xx"))
      <*> (fromRightS <$> decode <$> B.readFile (dir ++ "/tt"))

compVal :: ValR IO a → (a → IO b) → IO (ValR IO b)
compVal v f = do
  cur ← f =<< readValR v
  valRW ← newValRW cur
  onChangeValR v $ do
    nev ← f =<< readValR v
    writeValRW valRW nev
  pure $ toValR valRW

listVal :: Functor f ⇒ f [a] → [f a]
listVal v = generate $ \i → (!! i) <$> v

generate :: (Int → a) → [a]
generate f = f 0 : generate (f . (+1))

main :: IO ()
main = do
  datadir ← head <$> getArgs

  let n = 1000

  let t_max = fromIntegral (n-1)
  adj_t ← G.adjustmentNew 0 0 (fromIntegral t_max) 1 10 1
  let tVal = adjRW adj_t

  -- adj_wwr ← G.adjustmentNew 1 0 (fromIntegral t_max) 1 1000 1
  -- let wwRightBound = adjR adj_wwr


  αVal ← newValRW (head αVals)
  kkVal ← newValRW (head kkVals)
  ddVal ← newValRW (head ddVals)

  valsDirVal ← newValRW datadir

  dataVal ← compVal (toValR valsDirVal) $ \dir → do
    pars ← rpars <$> read <$> readFile (dir ++ "/pars")
    (axxs, atts) ← getvals dir
    pure (pars, axxs, atts)

  let parsVal = dataVal <&> \(pars,_,_) → pars
      axxsVal = dataVal <&> \(_,axxs,_) → axxs
      attsVal = dataVal <&> \(_,_,atts) → atts

  let tsVal = parsVal <&> \pars → \i → fromIntegral (i*(nn pars `div` n)) * dt pars

  let (xxsVal:ttsVal:wwsVal:_) =
        listVal $
        dataVal <&> \(pars,axxs,atts) →
        let xxs = \i → VU.slice (i*(ii pars+1)) (ii pars+1) axxs
            tts = \i → VU.slice (i*(ii pars+1)) (ii pars+1) atts
            wws = \i → VU.zipWith (ww pars) (xxs i) (tts i)
        in [xxs, tts, wws]
  {-
  let
    kVal = parsVal <&> \pars → nn pars `div` n
      tsVal  =
        (parsVal <×> kVal) <&> \(pars,k) →
        \i → fromIntegral (i*k) * dt pars
      xxsVal =
        (parsVal <×> axxsVal) <&> \(pars,axxs) →
        \i → VU.slice (i*(ii pars+1)) (ii pars+1) axxs
      ttsVal =
        (parsVal <×> attsVal) <&> \(pars,atts) →
        \i → VU.slice (i*(ii pars+1)) (ii pars+1) atts
      wwsVal =
        (parsVal <×> (xxsVal <×> ttsVal)) <&> \(pars,(xxs,tts)) →
        \i → VU.zipWith (ww pars) (xxs i) (tts i)
  -}

  wwMaxVal ← compVal (wwsVal) $ \(wws) →
    pure $! maximum $ flip map [0..n-1] $ \i → VG.maximum (VG.slice 0 (round (0.85*fromIntegral (VG.length (wws i)))) (wws i))

  let xxBounds = pure [0,0.2..1]
      ttBounds = pure [300,400..800]
      --wwBounds = wwRightBound <&> \r → [0,r]
      wwBounds = wwMaxVal <&> \wwMax → [0, wwMax]

  let psVal = (,,) <$> toValR αVal <*> toValR kkVal <*> toValR ddVal
  onChangeValR psVal $ do
    ps ← readValR psVal
    writeValRW valsDirVal (pdir ps)


  autoplay ← newValRW False

  G.initGUI
  window ← G.windowNew
  G.onDestroy window G.mainQuit
  G.set window
      [ G.windowTitle G.:= "cw"
      , G.containerBorderWidth G.:= 5
      , G.containerChild G.:=>
          G.vBoxNew False 5
          ⊭ ( G.hBoxNew False 5
            ⊭ createToggleButtonWithLabel autoplay "  ▸  "
            -- ⊨ G.hScrollbarNew adj_t
            ⊨ G.hScaleNew adj_t
            )
          ⊨ ( G.hBoxNew False 5
            ⊭ ( G.vBoxNew False 0
              -- ⊭ createLabel (pure $ "α = " ++ show (α pars0))
              ⊭ createLabel (parsVal <&> \pars → "α = " ++ showF 1 (α pars))
              ⊭ createLabel (parsVal <&> \pars → "K = " ++ showE 1 (kk pars) ++ " s⁻¹")
              ⊭ createLabel (parsVal <&> \pars → "D = " ++ showE 1 (dd pars) ++ " m²/s")
              ⊭ createLabel (parsVal <&> \pars → "E = " ++ showE 1 (ee pars) ++ " J/mol")
              ⊭ G.hSeparatorNew
              ⊭ createLabel ((tsVal <×> toValR tVal) <&> \(ts,t) → "t = " ++ showF 1 (ts (round t)) ++ " s")
              ⊭ createLabel (parsVal <&> \pars → show (method pars))
              ⊭ G.hSeparatorNew
              ⊭ createLabel (parsVal <&> \pars → "Δz = " ++ showE 2 (dz pars) ++ " m")
              ⊭ createLabel (parsVal <&> \pars → "I = " ++ show (ii pars))
              ⊭ createLabel (parsVal <&> \pars → "h = " ++ showE 2 (h pars) ++ " m")
              ⊭ G.hSeparatorNew
              ⊭ createLabel (parsVal <&> \pars → "Δt = " ++ showE 2 (dt pars) ++ " s")
              ⊭ createLabel (parsVal <&> \pars → "N = " ++ show (nn pars))
              ⊭ createLabel (parsVal <&> \pars → "T = " ++ showF 0 (fromIntegral (nn pars) * dt pars) ++ " s")
              ⊭ G.hSeparatorNew
              ⊭* createRadioButtons αVal (map (\v → (v, "α = " ++ showF 2 v)) αVals)
              ⊭ G.hSeparatorNew
              ⊭* createRadioButtons kkVal (map (\v → (v, "K = " ++ showE 1 v ++ " s⁻¹")) kkVals)
              ⊭ G.hSeparatorNew
              ⊭* createRadioButtons ddVal (map (\v → (v, "D = " ++ showE 2 v ++ " m²/s")) ddVals)
              )
            ⊨ createPlot "X"      n xxsVal xxBounds (toValR tVal)
            ⊨ createPlot "T, K"   n ttsVal ttBounds (toValR tVal)
            ⊨ createPlot "W, s⁻¹" n wwsVal wwBounds (toValR tVal)
            )
      ]

  -- autoplay
  timeoutP ← newIORef (Nothing :: Maybe G.HandlerId)
  onChangeValRW autoplay $ do
    readIORef timeoutP >>= \case
      Nothing → do
        timeout ← flip G.timeoutAdd 40 $ (const True <$>) $ do
          let k = 3
          t ← readValRW tVal
          let t' = t + k
          writeValRW tVal (if t' > fromIntegral t_max then 0 else t')
        writeIORef timeoutP $ Just timeout
      Just timeout → do
        G.timeoutRemove timeout
        writeIORef timeoutP Nothing

  G.widgetShowAll window
  G.mainGUI


{- pixmaps leak
gc ← G.gcNew =<< G.pixmapNew (Nothing :: Maybe G.Drawable) 1 1 (Just 24)
let (width, height) = (300, 600)
putStrLn "start"
forM_ [0..] $ \i → do
  print i
  getLine

  forM_ [1..100] $ \_ → do
    pixmap ← G.pixmapNew (Nothing :: Maybe G.Drawable) width height (Just 24)

    --let rtraces = traces tts (ttBounds,(0,fromIntegral hs-1)) (fromIntegral width, fromIntegral height)
    --G.renderWithDrawable pixmap rtraces
    G.drawRectangle pixmap gc True 0 0 (-1) (-1)
    finalizeForeignPtr $ Glib.unGObject $ Glib.toGObject pixmap
    pure ()
exitSuccess
-}
