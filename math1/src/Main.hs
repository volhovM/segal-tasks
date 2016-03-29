{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import qualified ConjGradient               as CG
import qualified Gauss                      as G
import           Types                      (SLAE, diagMatrix, fromSLAE,
                                             hilbert, solve)

import           Control.Lens
import qualified Graphics.Vty               as V

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
import           Control.Monad.IO.Class     (liftIO)
import           Numeric.LinearAlgebra.Data (Vector)

data MatrixType = Hilbert | Diagonal deriving (Show, Read)

getMatrixWithType :: MatrixType -> Int -> SLAE Double
getMatrixWithType Hilbert n = hilbert n
getMatrixWithType Diagonal n = diagMatrix n $ const 5

data St =
    St { _currentEditor :: T.Name
       , _edit1         :: E.Editor
       , _edit2         :: E.Editor
       }

$(makeLenses ''St)

firstEditor, secondEditor :: T.Name
firstEditor = "edit1"
secondEditor = "edit2"

switchEditors :: St -> St
switchEditors st =
    let next = if st^.currentEditor == firstEditor
               then secondEditor else firstEditor
    in st & currentEditor .~ next

currentEditorL :: St -> Lens' St E.Editor
currentEditorL st =
    if st^.currentEditor == firstEditor
    then edit1
    else edit2

drawUI :: St -> [T.Widget]
drawUI st = [ui]
  where
    ui =
        vBox
            [ str "Press Tab to switch between editors, Esc to quit."
            , hBorder
            , hBox
                  [ hLimit 40 $ vBox
                        [ (textField "Matrix type:" 10 1 (st ^. edit1))
                        , str ""
                        , (textField "Matrix size:" 10 1 (st ^. edit2))
                        , hBorder
                        , C.center (str "Previeooha")]
                  , vBorder
                  , C.center (str "Тут будет матрица")]]
    textField t n m inner =
        str t <=> str "" <=> (hLimit (max n $ length t) $ vLimit m $ E.renderEditor inner)

appEvent :: St -> V.Event -> T.EventM (T.Next St)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> halt st
        V.EvKey (V.KEnter) [] | st ^. currentEditor == secondEditor -> do
            let tp = (read $ head $ E.getEditContents $ st ^. edit1) :: MatrixType
                sz = (read $ head $ E.getEditContents $ st ^. edit2) :: Int
            (morphedMatrix :: G.GaussMatrix) <-
                liftIO $ fromSLAE $ getMatrixWithType tp sz
            (solution :: Vector Double) <- liftIO $ solve morphedMatrix
            continue $ switchEditors st
        V.EvKey (V.KChar '\t') [] -> continue $ switchEditors st
        V.EvKey V.KBackTab [] -> continue $ switchEditors st
        _ -> continue =<< T.handleEventLensed st (currentEditorL st) ev

initialState :: St
initialState =
    St firstEditor
       (E.editor firstEditor (str . unlines) Nothing "")
       (E.editor secondEditor (str . unlines) (Just 2) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    ]

appCursor :: St -> [T.CursorLocation] -> Maybe T.CursorLocation
appCursor st = showCursorNamed (st^.currentEditor)

theApp :: App St V.Event
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
