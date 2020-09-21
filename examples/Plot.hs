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
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Diagrams.Prelude hiding (set)
import Diagrams.Size (requiredScaling)
import Diagrams.Backend.Cairo (Cairo)
import qualified Data.Colour as C

-- The classic Hilbert curves from the diagrams gallery:
hilbert :: Int -> Diagram Cairo
hilbert = frame 1 . lw medium . lc (colors!!1) . strokeT . mempty

-- Some more drawing code, copied from
-- projects.haskell.org/diagrams/gallery/Pentaflake.html
colors ::[Colour Double]
colors = iterate (C.blend 0.1 white) red

p :: Diagram Cairo
p = regPoly 5 1 # lwO 0

-- A function to set up the main window and signal handlers
createMainWindow :: [(String, [Double])] -> IO Gtk.Window
createMainWindow yss = do
    win <- new Gtk.Window []

    on win #keyPressEvent $ \event -> do
        name <- event `get` #keyval >>= Gdk.keyvalName
        when (name == Just "Escape") Gtk.mainQuit
        return False

    drawArea <- new Gtk.DrawingArea [#widthRequest := 512, #heightRequest := 512]

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
        let dia = renderAxis $ plotdata yss
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
main :: [(String, [Double])] -> IO ()
main yss = do
    Gtk.init Nothing
    win <- createMainWindow yss
    on win #destroy Gtk.mainQuit
    Gtk.widgetShowAll win
    Gtk.main

plotdata yss =
  let datapoints = map (\(lbl, ys) -> (lbl, zip [1..] ys)) yss
  in r2Axis &~ do
    mapM (\(lbl, points) -> linePlot points (key lbl)) datapoints

mydata1 = [(1,3), (2,5.5), (3.2, 6), (3.5, 6.1)]
mydata2 = mydata1 & each . _1 *~ 0.5
mydata3 = [V2 1.2 2.7, V2 2 5.1, V2 3.2 2.6, V2 3.5 5]

myaxis = r2Axis &~ do
  linePlot' mydata1
  linePlot mydata2 $ do
    key "data 2"
    plotColor .= black

  linePlot mydata3 $ key "data 3"
