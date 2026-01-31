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

import FrenchQuote
import French
import Returns
import Tools

import Control.Monad (when)
import Data.Colour (Colour)
import Data.Colour.Names
import Data.Default (def)
import Data.List (sort)
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Debug.Trace
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Axis.Floating
import Graphics.Rendering.Chart.Backend.Cairo (toFile, FileOptions(..), FileFormat(..))


-- | Default color palette for multiple lines
defaultColors :: [Colour Double]
defaultColors =
    [ blue
    , red
    , orange
    , purple
    , cyan
    , magenta
    , green
    , brown
    , pink
    , olive
    ]


-- | Create line plot data from periods and values
makeLinePlot :: [Period] -> (String, [Double]) -> Colour Double
             -> PlotLines Day Double
makeLinePlot periods (name, values) color =
    plot_lines_title .~ name
  $ plot_lines_style . line_color .~ opaque color
  $ plot_lines_style . line_width .~ 2.0
  $ plot_lines_values .~ [zip (map periodToDay periods) values]
  $ def


plotLineGraphInner logScale filePath title yLabel periods dataSeries =
  let ylabel = "Date"
      colors = cycle defaultColors

      -- Clip the data series to all cover the same date range
      names = map fst dataSeries
      histories = intersectDates $ map snd dataSeries
      clippedDataSeries = zip names $ toLists histories

      axisPeriods = case maybePeriods (head histories) of
        Just p -> p
        Nothing -> sort periods

      plots = zipWith (makeLinePlot axisPeriods) clippedDataSeries colors
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
--   - periods: List of Periods. for x-axis. If dataSeries contain
--     periods, this is ignored and dataSeries' period intersection is
--     used instead.
--   - dataSeries: List of (name, values) pairs for each line
--
-- (There is no X-axis label parameter because the X axis is always dates.)
--
-- Example:
-- @
-- plotLineGraph
--     "output.svg"
--     "Monthly Sales"
--     "Revenue ($)"
--     [Period 2024 1, Period 2024 2, Period 2024 3, Period 2024 4]
--     [ ("Product A", [100, 150, 130, 180])
--     , ("Product B", [80, 90, 120, 140])
--     , ("Product C", [60, 85, 95, 110])
--     ]
-- @
plotLineGraph :: (ReturnsHistory a, Show a)
              => FilePath      -- ^ Output file (PNG)
              -> String        -- ^ Chart title
              -> String        -- ^ Y-axis label
              -> [Period]      -- ^ Dates
              -> [(String, a)] -- ^ Named data series
              -> IO ()
plotLineGraph = plotLineGraphInner False


-- | Generate a line graph on a logarithmic scale and save to file.
plotLineGraphLog :: ReturnsHistory a
                 => FilePath     -- ^ Output file
                 -> String       -- ^ Chart title
                 -> String       -- ^ Y-axis label
                 -> [Period]     -- ^ Dates
                 -> [(String, a)] -- ^ Named data series
                 -> IO ()
plotLineGraphLog = plotLineGraphInner True
