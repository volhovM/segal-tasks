module Glade (MainWindow(..)) where

import qualified Graphics.UI.Gtk as Gtk

data MainWindow = MainWindow
    { mwWindow :: Gtk.Window
    }
