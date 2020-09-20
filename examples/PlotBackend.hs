{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Gtk
-- Copyright   :  (c) 2019 Torsten Kemps-Benedix
-- License     :  MIT-style (see LICENSE)
-- Maintainer  :  tkx68@icloud.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Convenient interface to rendering diagrams directly
-- on Gtk DrawingArea widgets using the Cairo backend. This package uses Cairo
-- double buffering (see <https://developer.gnome.org/gtk3/stable/chap-drawing-model.html#double-buffering>).
--
-- See the following example for a practical use case. Have a close look at the
-- use of 'renderToGtk' in the `on drawArea #draw` code block. See
-- <https://hackage.haskell.org/package/gi-gtk-3.0.27/docs/GI-Gtk-Objects-Widget.html#g:47>
-- for details on the draw signal or
-- <https://developer.gnome.org/gtk3/stable/GtkWidget.html#GtkWidget-draw>
-- for the original GTK3 documentation.
--
-- @
-- {-# LANGUAGE OverloadedLabels, OverloadedStrings, TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}
-- module Main where
--
-- import Control.Monad
-- import qualified GI.Gdk as Gdk
-- import qualified GI.Gtk as Gtk
-- import Data.GI.Base
-- import Diagrams.Prelude hiding (set)
-- import Diagrams.Size (requiredScaling)
-- import Diagrams.Backend.GIGtk
-- import Diagrams.Backend.Cairo (Cairo)
-- import qualified Data.Colour as C
-- import Data.Text (Text)
-- import qualified Data.Text as T
--
-- hilbert :: Int -> Diagram Cairo
-- hilbert = frame 1 . lw medium . lc (colors!!1) . strokeT . hilbert'
--   where
--     hilbert' :: Int -> Trail V2 Double
--     hilbert' 0 = mempty
--     hilbert' n =
--         hilbert'' (n-1) # reflectY <> vrule 1
--         <> hilbert'  (n-1) <> hrule 1
--         <> hilbert'  (n-1) <> vrule (-1)
--         <> hilbert'' (n-1) # reflectX
--       where
--         hilbert'' :: Int -> Trail V2 Double
--         hilbert'' m = hilbert' m # rotateBy (1/4)
--
-- -- Our drawing code, copied from
-- -- projects.haskell.org/diagrams/gallery/Pentaflake.html
-- colors ::[Colour Double]
-- colors = iterate (C.blend 0.1 white) red
--
-- p ::Diagram Cairo
-- p = regPoly 5 1 # lwO 0
--
-- -- | create a snowflake diagram of depth @n@
-- --
-- -- specifying a type here because the monoidal query type needs to be specified
-- -- for @drawToGtk@, otherwise get a "No instance for (PathLike ..." error.
-- pentaflake :: Int -> Diagram Cairo
-- pentaflake 0 = p
-- pentaflake n = appends (p' # fc (colors !! (n-1)))
--                        (zip vs (repeat (rotateBy (1/2) p')))
--   where vs = take 5 . iterate (rotateBy (1/5))
--                     . (if odd n then negated else id) $ unitY
--         p' = pentaflake (n-1)
--
-- pentaflake' ::Int -> Diagram Cairo
-- pentaflake' n = pentaflake n # fc (colors !! n)
--
-- -- end of diagrams code
--
-- -- A function to set up the main window and signal handlers
-- createMainWindow :: IO Gtk.Window
-- createMainWindow = do
--     win <- new Gtk.Window []
--
--     on win #keyPressEvent $ \event -> do
--         name <- event `get` #keyval >>= Gdk.keyvalName
--         when (name == Just "Escape") Gtk.mainQuit
--         return False
--
--     depthWidget <- Gtk.spinButtonNewWithRange 1 10 1
--     -- when the spinButton changes, redraw the window
--     on depthWidget #valueChanged $ do
--         Gtk.widgetQueueDraw win --drawArea
--         return ()
--
--     rbHilbert <- Gtk.radioButtonNewWithLabelFromWidget Gtk.noRadioButton "Hilbert"
--     set rbHilbert [#active := True]
--     rbPentaFlake <- Gtk.radioButtonNewWithLabelFromWidget (Just rbHilbert) "Penta Flake"
--     set rbPentaFlake [#active := False]
--     boxRB <- Gtk.boxNew Gtk.OrientationVertical 0
--     Gtk.boxPackStart boxRB rbHilbert False False 0
--     Gtk.boxPackStart boxRB rbPentaFlake False False 0
--
--     drawArea <- new Gtk.DrawingArea [#widthRequest := 512, #heightRequest := 512]
--
--     -- add the depthWidget control and drawArea to the main window
--     hbox <- Gtk.boxNew Gtk.OrientationVertical 0
--     Gtk.boxPackStart hbox boxRB False False 0 -- box child expand fill extraPadding
--     Gtk.boxPackStart hbox depthWidget False False 0 -- box child expand fill extraPadding
--     Gtk.boxPackStart hbox drawArea True True 0
--     #add win hbox
--
--     on rbHilbert #toggled $ do
--         Gtk.widgetQueueDraw drawArea
--         return ()
--
--     -- handle the drawArea's @onExpose@ signal.  We provide a function
--     -- that takes an area marked as dirty and redraws it.
--     -- This program simply redraws the entire drawArea.
--     --
--     -- Many gtk signal handlers return True if the signal was handled, and False
--     -- otherwise (in which case the signal will be propagated to the parent).
--     on drawArea #draw $ \context -> do
--         rect <- Gtk.widgetGetAllocation drawArea  -- size in pixels (Int)
--         canvasX <- get rect #width
--         canvasY <- get rect #height
--         curDepth <- fromIntegral <$> Gtk.spinButtonGetValueAsInt depthWidget
--         hilbertActive <- get rbHilbert #active
--         let dia = if hilbertActive then hilbert curDepth else pentaflake curDepth
--             w = width dia
--             h = height dia
--             spec = mkSizeSpec2D (Just $ fromIntegral canvasX) (Just $ fromIntegral canvasY)
--             scaledDia = toGtkCoords $ transform (requiredScaling spec (V2 w h)) dia
--         renderToGtk context True scaledDia
--         return True
--
--     return win
--
-- -- Gtk application
-- --
-- -- Initialize the library, create and show the main window,
-- -- finally enter the main loop
-- main :: IO ()
-- main = do
--     Gtk.init Nothing
--     win <- createMainWindow
--     on win #destroy Gtk.mainQuit
--     Gtk.widgetShowAll win
--     Gtk.main
-- @
-----------------------------------------------------------------------------
module PlotBackend
       ( defaultRender
       , toGtkCoords
       , renderToGtk
       ) where

import           Control.Monad.Trans.Reader (runReaderT)
import           Diagrams.Prelude hiding (render, height, width)
import           Diagrams.Backend.Cairo.Internal
import           Foreign.Ptr (castPtr)
import           GHC.Int
import qualified GI.Cairo (Context(..))
import           GI.Gtk
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo (Render(runRender))
import qualified Graphics.Rendering.Cairo.Types as Cairo (Cairo(Cairo))

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Cairo.Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (Cairo.runRender r) (Cairo.Cairo (castPtr p))

-- | Convert a Diagram to the backend coordinates.
--
-- Provided to Query the diagram with coordinates from a mouse click
-- event.
--
-- > widget `on` buttonPressEvent $ tryEvent $ do
-- >   click <- eventClick
-- >   (x,y) <- eventCoordinates
-- >   let result = runQuery (query $ toGtkCoords myDiagram) (x ^& y)
-- >   do_something_with result
--
-- `toGtkCoords` does no rescaling of the diagram, however it is centered in
-- the window.
toGtkCoords :: Monoid' m => QDiagram Cairo V2 Double m -> QDiagram Cairo V2 Double m
toGtkCoords d = (\(_,_,d') -> d') $
  adjustDia Cairo
            (CairoOptions "" absolute RenderOnly False)
            d

-- | Render a diagram to a 'DrawingArea''s context with double buffering if needed,
--   rescaling to fit the full area.
defaultRender ::
     Monoid' m =>
    GI.Cairo.Context -- ^ DrawingArea's context to render onto --  provided by the draw event
    -> Bool -- ^render double buffered?
    -> QDiagram Cairo V2 Double m   -- ^ Diagram
    -> IO ()
defaultRender ctx diagram = do
  render ctx opts diagram
    where opts w h = (CairoOptions
              { _cairoFileName     = ""
              , _cairoSizeSpec     = dims (V2 (fromIntegral w) (fromIntegral h))
              , _cairoOutputType   = RenderOnly
              , _cairoBypassAdjust = False
              }
           )

-- | Render a diagram to a 'DrawArea''s context with double buffering.  No
--   rescaling or transformations will be performed.
--
--   Typically the diagram will already have been transformed by
--   'toGtkCoords'.
renderToGtk ::
  (Monoid' m)
  => GI.Cairo.Context -- ^ DrawingArea's context to render onto --  provided by the draw event
  -> Bool -- ^render double buffered?
  -> QDiagram Cairo V2 Double m  -- ^ Diagram
  -> IO ()
renderToGtk ctx db = render ctx opts db
  where opts _ _ = (CairoOptions
                    { _cairoFileName     = ""
                    , _cairoSizeSpec     = absolute
                    , _cairoOutputType   = RenderOnly
                    , _cairoBypassAdjust = True
                    }
                   )

-- | Render a diagram onto a 'GI.Cairo.Context' using the given CairoOptions. Place this within a 'draw' event callback which provides the DrawArea's context.
--
--   This uses cairo double-buffering if the thirs parameter is set to True..
render ::
  (Monoid' m) =>
  GI.Cairo.Context -- ^ DrawingArea's 'GI.Cairo.Context' to render the digram onto
  -> (Int32 -> Int32 -> Options Cairo V2 Double) -- ^ options, depending on drawable width and height
  -> Bool -- ^render double buffered?
  -> QDiagram Cairo V2 Double m -- ^ Diagram
  -> IO ()
render ctx renderOpts db diagram =
    renderWithContext ctx (do
        (x1, x2, y1, y2) <- Cairo.clipExtents
        let w = round $ x2 - x1
            h = round $ y2 - y1
            opts = renderOpts w h
        if db
            then doubleBuffer $ do
                delete w h
                snd (renderDia Cairo opts diagram)
            else
                snd (renderDia Cairo opts diagram)
    )

--
--   Used to clear canvas when using double buffering.
delete :: Int32 -> Int32 -> Cairo.Render ()
delete w h = do
  Cairo.setSourceRGB 1 1 1
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.fill


-- | Wrap the given render action in double buffering.
doubleBuffer :: Cairo.Render () -> Cairo.Render ()
doubleBuffer renderAction = do
  Cairo.pushGroup
  renderAction
  Cairo.popGroupToSource
  Cairo.paint
