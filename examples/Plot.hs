{- | A first example of drawing diagrams from within GTK.  This
     program draws a Koch snowflake with the depth controllable
     via a GTK widget.

     Install dependencies:
     - stack install gi-gtk plots diagrams-cairo
-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}
module Plot where

import PlotBackend

import Plots

import Control.Monad
import Control.DeepSeq

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Diagrams.Prelude hiding (set)
import Diagrams.Size (requiredScaling)
import Diagrams.Backend.Cairo (Cairo)
import qualified Data.Colour as C

-- A function to set up the main window and signal handlers
createMainWindow :: [(String, [Double])] -> IO Gtk.Window
createMainWindow yss = do
    win <- new Gtk.Window []

    on win #keyPressEvent $ \event -> do
        name <- event `get` #keyval >>= Gdk.keyvalName
        when (name == Just "Escape") Gtk.mainQuit
        return False

    drawArea <- new Gtk.DrawingArea [#widthRequest := 1000, #heightRequest := 1000]

    -- add the depthWidget control and drawArea to the main window
    hbox <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.boxPackStart hbox drawArea True True 0
    #add win hbox

    -- handle the drawArea's @onExpose@ signal.  We provide a function
    -- that takes an area marked as dirty and redraws it.
    -- This program simply redraws the entire drawArea.
    --
    -- Many gtk signal handlers return True if the signal was handled, and False
    -- otherwise (in which case the signal will be propagated to the parent).
    on drawArea #draw $ \context -> do
        rect <- Gtk.widgetGetAllocation drawArea  -- size in pixels (Int)
        canvasX <- get rect #width
        canvasY <- get rect #height
        let dia = renderAxis $ linesPlot yss
            w = width dia
            h = height dia
            spec = mkSizeSpec2D (Just $ fromIntegral canvasX) (Just $ fromIntegral canvasY)
            scaledDia_ = toGtkCoords $ transform (requiredScaling spec (V2 w h)) dia
        renderToGtk context True scaledDia_  -- (renderAxis myaxis) -- scaledDia
        return True

    return win

-- Gtk application
--
-- Initialize the library, create and show the main window,
-- finally enter the main loop
doLinesPlot :: [(String, [Double])] -> IO ()
doLinesPlot yss = do
    deepseq yss $ return ()
    Gtk.init Nothing
    win <- createMainWindow yss
    on win #destroy Gtk.mainQuit
    Gtk.widgetShowAll win
    Gtk.main

test :: IO ()
test = do
  Gtk.init Nothing
  win <- createMainWindow undefined
  Gtk.widgetShowAll win
  Gtk.main

linesPlot yss =
  let datapoints = map (\(lbl, ys) -> (lbl, zip [1..] ys)) yss
  in r2Axis &~ do
    mapM (\(lbl, points) -> linePlot points (key lbl)) datapoints
    -- smoothLinePlot
