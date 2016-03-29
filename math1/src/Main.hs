{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import qualified ConjGradient               as CG
import qualified Gauss                      as G
import           Types                      (SLAE, diagMatrix, fromSLAE,
                                             hilbert, solve)

import qualified Brick.AttrMap              as A
import           Brick.Main                 (App (..), continue, defaultMain,
                                             halt, showCursorNamed)
import qualified Brick.Types                as T
import           Brick.Util                 (on)
import           Brick.Widgets.Border       (hBorder, vBorder)
import qualified Brick.Widgets.Center       as C
import           Brick.Widgets.Core         (hBox, hLimit, padAll, str, vBox,
                                             vLimit, (<+>), (<=>))
import qualified Brick.Widgets.Edit         as E
import           Control.Lens               (Lens', makeLenses, (&), (.~), (^.))
import           Control.Monad.IO.Class     (liftIO)
import           Data.Maybe                 (fromJust, fromMaybe, isJust)
import qualified Graphics.Vty               as V
import           Numeric.LinearAlgebra.Data (Vector, asRow, disps)

data MatrixType
    = Hilbert
    | Diagonal
    deriving (Show,Read)

getMatrixWithType :: MatrixType -> Int -> SLAE Double
getMatrixWithType Hilbert n = hilbert n
getMatrixWithType Diagonal n = diagMatrix n $ const 5

data AppState = AppState
    { _currentEditor  :: T.Name
    , _chosenMatType  :: Maybe MatrixType
    , _chosenSize     :: Maybe Int
    , _renderedMatrix :: String
    , _answers        :: [(String, String)]
    , _edit1          :: E.Editor
    , _edit2          :: E.Editor
    }

$(makeLenses ''AppState)

firstEditor, secondEditor :: T.Name
firstEditor = "edit1"
secondEditor = "edit2"

switchEditors :: AppState -> AppState
switchEditors st =
    let next = if st^.currentEditor == firstEditor
               then secondEditor else firstEditor
    in st & currentEditor .~ next

currentEditorL :: AppState -> Lens' AppState E.Editor
currentEditorL st =
    if st^.currentEditor == firstEditor
    then edit1
    else edit2

drawUI :: AppState -> [T.Widget]
drawUI st = [ui]
  where
    ui =
        vBox
            [ (str "Current state: " <=>
               hBox
                   [ (textField "Matrix type:" 10 1 (st ^. edit1))
                   , str " "
                   , (textField "Matrix size:" 10 1 (st ^. edit2))])
            , hBorder
            , vBox
                  [ C.center (str $ st ^. renderedMatrix)
                  , hBorder
                  , vLimit 10 $
                    C.center
                        (str $
                         unlines $
                         map (\(a,b) -> a ++ ": " ++ b) $
                         st ^. answers)]
            , hBorder
            , str "Press Tab to switch between editors, Esc to quit."]
    textField t n m inner =
        str t <+>
        str " " <+>
        (hLimit (max n $ length t) $ vLimit m $ E.renderEditor inner)

appEvent :: AppState -> V.Event -> T.EventM (T.Next AppState)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> halt st
        V.EvKey V.KEnter []
          | st ^. currentEditor == firstEditor -> do
              let tp =
                      (read $ head $ E.getEditContents $ st ^. edit1) :: MatrixType
              continue $
                  switchEditors $
                  st & renderedMatrix .~ show
                           (getMatrixWithType tp (fromMaybe 5 $ st ^. chosenSize))
                     & chosenMatType .~ (Just tp)

        V.EvKey V.KEnter []
          | st ^. currentEditor == secondEditor && isJust (st ^. chosenMatType) -> do
              let sz = (read $ head $ E.getEditContents $ st ^. edit2) :: Int
                  tp = fromJust $ st ^. chosenMatType
                  initMatrix = getMatrixWithType tp sz
              (morphedMatrix :: G.GaussMatrix) <-
                  liftIO $
                  fromSLAE initMatrix
              (solution :: Vector Double) <- liftIO $ solve morphedMatrix
              continue $
                  switchEditors $ st
                      & chosenSize .~ Just sz
                      & renderedMatrix .~ show initMatrix
                      & answers .~ [("Gauss", disps 3 $ asRow solution)]
        V.EvKey (V.KChar '\t') [] -> continue $ switchEditors st
        V.EvKey V.KBackTab [] -> continue $ switchEditors st
        _ -> continue =<< T.handleEventLensed st (currentEditorL st) ev

initialState :: AppState
initialState =
    AppState
        firstEditor
        (Just Hilbert)
        (Just 5)
        ""
        []
        (E.editor firstEditor (str . unlines) Nothing "")
        (E.editor secondEditor (str . unlines) (Just 2) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    ]

appCursor :: AppState -> [T.CursorLocation] -> Maybe T.CursorLocation
appCursor st = showCursorNamed (st^.currentEditor)

theApp :: App AppState V.Event
theApp =
    App
    { appDraw = drawUI
    , appChooseCursor = appCursor
    , appHandleEvent = appEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    , appLiftVtyEvent = id
    }

main :: IO ()
main = do
    st <- defaultMain theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit1
    putStrLn "In input 2 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit2
