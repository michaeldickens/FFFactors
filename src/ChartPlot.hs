{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ChartPlot
Description : Make line graphs from price/return series.

Maintainer  : Michael Dickens
Created     : 2026-01-26

Originally written by Claude Opus 4.5.

-}

module ChartPlot
    ( plotLineGraph
    , plotLineGraphLog
    ) where

import Quote
import Period

import Control.Monad (when)
import Data.Colour()
import Data.Colour.SRGB (sRGB24)
import Data.List (sort)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Graphics.Rendering.Chart.Easy hiding (colors)
import Graphics.Rendering.Chart.Backend.Cairo (toFile, FileOptions(..), FileFormat(..))


-- | Default color palette for multiple lines
-- Colors chosen for high contrast, colorblind accessibility, and easy differentiation
defaultColors :: [Colour Double]
defaultColors =
    [ sRGB24 0 95 168    -- Deep teal (vibrant, distinct)
    , sRGB24 255 112 67  -- Coral orange (warm contrast)
    , sRGB24 17 46 81    -- Navy blue (dark, professional)
    , sRGB24 155 29 32   -- Deep red/burgundy (strong, accessible)
    , sRGB24 61 90 128   -- Slate blue (cool mid-tone)
    , sRGB24 120 144 156 -- Blue-grey (neutral)
    , sRGB24 139 87 42   -- Brown (earthy, distinct)
    , sRGB24 148 0 211   -- Deep purple (vibrant)
    , sRGB24 0 128 128   -- Teal (darker than first)
    , sRGB24 107 142 35  -- Olive green (muted, natural)
    ]


-- | Create line plot data from periods and values
makeLinePlot :: (String, RetSeries) -> Colour Double
             -> PlotLines Day Double
makeLinePlot (name, values) color =
    plot_lines_title .~ name
  $ plot_lines_style . line_color .~ opaque color
  $ plot_lines_style . line_width .~ 2.0
  $ plot_lines_values .~
  [map (\(k, v) -> (periodToDay k, v)) $ sort $ Map.toList values]
  $ def


plotLineGraphInner :: Bool -> FilePath -> String -> String -> [(String, RetSeries)] -> IO ()
plotLineGraphInner logScale filePath title yLabel dataSeries =
  let colors = cycle defaultColors

      plots = zipWith (makeLinePlot) dataSeries colors

      imgFormat = case last $ Text.splitOn "." $ Text.pack filePath of
        "png" -> PNG
        "svg" -> SVG
        "pdf" -> PDF
        "ps"  -> PS
        _     -> error $ "plotLineGraph: Unsupported file extension: " ++ filePath

      fileOpts = def { _fo_format = imgFormat, _fo_size = (1200, 800) }

      legendStyle = legend_orientation .~ LORows 3
                    $ legend_margin .~ 10
                    $ legend_label_style . font_size .~ 24
                    $ def

  in toFile fileOpts filePath $ do
      layout_title .= title
      layout_title_style . font_size .= 28
      layout_y_axis . laxis_title .= yLabel
      layout_y_axis . laxis_title_style . font_size .= 24  -- axis name
      when logScale
        $ layout_y_axis . laxis_generate .= autoScaledLogAxis def
      layout_x_axis . laxis_style . axis_label_style . font_size .= 24  -- axis ticks
      layout_y_axis . laxis_style . axis_label_style . font_size .= 24
      layout_legend ?= legendStyle

      mapM_ (\p -> plot (return $ toPlot p)) plots


-- | Generate a line graph and save to file.
--
-- Parameters:
--   - filePath: Output file path (supports .svg, .png, .pdf based on extension).
--   - title: Chart title.
--   - yLabel: Y-axis label.
--   - dataSeries: List of (name, values) pairs for each line
--
-- (There is no X-axis label parameter because the X axis is always dates.)
plotLineGraph :: FilePath               -- ^ Output file (PNG)
              -> String                 -- ^ Chart title
              -> String                 -- ^ Y-axis label
              -> [(String, RetSeries)] -- ^ Named data series
              -> IO ()
plotLineGraph = plotLineGraphInner False


-- | Generate a line graph on a logarithmic scale and save to file.
plotLineGraphLog :: FilePath               -- ^ Output file
                 -> String                 -- ^ Chart title
                 -> String                 -- ^ Y-axis label
                 -> [(String, RetSeries)] -- ^ Named data series
                 -> IO ()
plotLineGraphLog = plotLineGraphInner True
