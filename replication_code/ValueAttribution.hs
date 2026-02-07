{-# OPTIONS_GHC
    -fno-warn-x-partial
#-}
{- |
Module      : ValueAttribution
Description : Replication code for "Value investing is overrated by value investors"

Maintainer  : Michael Dickens
Created     : 2026-02-06

-}

module ValueAttribution where

import French
import Returns

import Control.Monad
import Data.Foldable (for_)
import Data.Function (on)
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Text.Printf


data PrintType = Concise | Verbose | Graph deriving (Eq)


-- | Given a map of breakpoints for some fundamental-to-price ratio, approximate
-- the average valuation for stocks that fall within the given percentile range.
-- For example, for a percentile range of (70, 100), find the approximate
-- average fundamental-to-price for the cheapest 30% of stocks.
--
-- The exact correct value cannot be computed without individual stock data, so
-- this function uses the trapezoid rule to approximate the answer.
getAverageFromBreakpoints :: QuoteMap -> (Int, Int) -> RetSeries
getAverageFromBreakpoints breakpoints (percentileMin, percentileMax) =
  let percentiles = filter (\x -> x >= percentileMin && x <= percentileMax) $ map (* 5) [0..20]
  in Map.map
  (\slice ->
     -- take the sum using the trapezoid rule (using geometric mean instead of
     -- arithmetic)
     let divisor = fromIntegral (percentileMax - percentileMin) / 100
         values = map (\k ->
                         -- 0th percentile is not provided, but by construction,
                         -- 0th percentile is always 0
                         if k == 0
                         then 0
                         else slice Map.! (Text.pack $ printf "%dth percentile" k)
                      ) percentiles
     in fromJust $ sum $ zipWith (\x y -> sqrt (x * y) * (0.05 / divisor)) values (tail values)
  )
  breakpoints


-- | Value factor attribution broken out into valuation change + structural
-- return.
valueAttribution :: String -> Int -> Int -> PrintType -> IO ()
valueAttribution metric' startYear endYear printType = do
  quotes <- loadDB $ (printf "French/Portfolios_%s_Value_Wt.csv" metric' :: String)
  breakpoints <- loadDB $ (printf "French/Breakpoints_%s.csv" metric' :: String)

  let metric = map (\c -> if c == '-' then '/' else c) metric'

  -- To match Arnott et al. (2021) and French breakpoints, years should end in
  -- June
  let endMonth = 6

  let normalize x = 100 * (log x) / (fromIntegral $ endYear - startYear)

  let getMultiples :: (Int, Int) -> Map.HashMap Int Double
      getMultiples percentileRange =
        -- Invert, so it's price/fundamental instead of fundamental/price.
        -- BM breakpoints are not percentages, unlike the others, but we
        -- don't need to multiply by 100 because the division operations later
        -- will cancel out the 100s anyway.
        Map.fromList $ map (\(p, v) -> (year p, 1 / v)) $ Map.toList
        $ getAverageFromBreakpoints breakpoints percentileRange

  -- Prices plus cumulative dividends
  let retsToYearlyPrices :: RetSeries -> Map.HashMap Int Double
      retsToYearlyPrices rets =
        let monthlyPrices = zip (getDateRange rets) (returnsToPrices $ toList rets)
        in Map.fromList $ map (\(p, r) -> (year p, r)) $ sortBy (compare `on` fst)
            $ filter (\(p, _) -> month p == endMonth) monthlyPrices

  -- Get prices and (estimated) multiples for the top 30% and bottom 30% of the
  -- market, excluding companies with negative fundamentals. (I would rather not
  -- exclude them, but I have to because of how the French breakpoints work)
  let growthMultiples = getMultiples (0, 30)
  let valueMultiples = getMultiples (70, 100)
  let growthPrices = retsToYearlyPrices $ getRets1 "Lo 30" quotes
  let valuePrices = retsToYearlyPrices $ getRets1 "Hi 30" quotes

  let hmlMultiples' = Map.intersectionWith (/) growthMultiples valueMultiples
  let hmlMultiples = Map.filterWithKey (\k _ -> k >= startYear && k <= endYear) hmlMultiples'

  -- Don't calculate using `getRets` because that will rebalance monthly. We
  -- want annual rebalancing to match how HML is constructed
  let hmlPrices = Map.intersectionWith (/) valuePrices growthPrices

  let getFundamentals prices multiples = Map.intersectionWith (/) prices multiples

  -- Technically, these are adjusted for dividends
  let growthFundamentals = getFundamentals growthPrices growthMultiples
  let valueFundamentals = getFundamentals valuePrices valueMultiples
  let hmlFundamentals = Map.intersectionWith (/) valueFundamentals growthFundamentals

  when (printType == Verbose) $
    printf "=== Value Factor Attribution (%s), %d to %d ===\n\n" metric startYear endYear

  when (printType /= Verbose) $ do
    printf "%4s -- %d to %d: " metric startYear endYear
    printf "%4.1f%% return = %4.1f%% valuation + %4.1f%% structural\n"
      (normalize $ (hmlPrices ! endYear) / (hmlPrices ! startYear))
      (negate $ normalize $ (hmlMultiples ! endYear) / (hmlMultiples ! startYear))
      (normalize $ (hmlFundamentals ! endYear) / (hmlFundamentals ! startYear))

  -- print regression of 10-year returns on valuation spreads
  when (printType == Verbose && (endYear - 10 > startYear)) $ do
    let forwardRets = map (\y -> (hmlPrices!(y + 10) / hmlPrices!y)**(1/10) - 1) [startYear..(endYear - 10)]
    putStr "\tregression of 10-year returns on starting valuation spreads: \n\t\t"
    printLinearRegression (map (hmlMultiples!) [startYear..(endYear-10)]) forwardRets

  -- graph drawdowns and rolling returns
  when (printType == Graph) $ do
    let priceDD = rebuild drawdowns $ Map.mapKeys yearToPeriod $ pricesToReturns hmlPrices
    let multipleDD = rebuild drawdowns $ Map.mapKeys yearToPeriod $ pricesToReturns $ Map.map (1 /) hmlMultiples
    let structuralDD = rebuild drawdowns $ Map.mapKeys yearToPeriod $ pricesToReturns hmlFundamentals
    plotLineGraph (printf "images/value attribution drawdowns (%s).png" metric')
      (printf "Value Attribution Drawdowns (%s)" metric)
      "Drawdown"
      [ ("Structural", structuralDD)
      , ("Valuation Multiple", multipleDD)
      , ("Total Return", priceDD)
      ]
    plotLineGraph (printf "images/structural drawdowns (%s).png" metric')
      (printf "Value Structural Drawdowns (%s)" metric)
      "Drawdown"
      [("", structuralDD)]

    let rollingRets =
          Map.fromList
          $ map (\xs -> (Period (fst $ last xs) defaultMonth, geometricMean $ map snd xs))
          $ map (take 15)
          $ takeWhile (\xs -> length xs >= 15) $ tails
          $ sortBy (compare `on` fst)
          $ Map.toList $ pricesToReturns hmlFundamentals
    plotLineGraph (printf "images/structural rolling returns (%s).png" metric')
      (printf "Rolling 15-Year Structural Returns (%s)" metric)
      "Rolling Return"
      [ ("", rollingRets)
      ]


main = do
  valueAttribution "B-M" 1927 2025 Graph
  -- valueAttribution "E-P" 1952 2025 Graph
  -- valueAttribution "CF-P" 1952 2025 Graph

  -- plotLineGraph "images/value structural return.png"
  --   "Value Structural Return (HML)"
  --   "Price"
  --   [ ("B/M", bm)
  --   , ("E/P", ep)
  --   , ("CF/P", cfp)
  --   ]

  putStrLn ""
  valueAttribution "B-M" 1927 2006 Concise
  valueAttribution "E-P" 1952 2006 Concise
  valueAttribution "CF-P" 1963 2006 Concise

  -- It makes a big difference whether you end in 2019 or 2020 or 2021 because
  -- those were some wild years for value. 2020 had a huge structural drawdown
  -- and 2021 had a huge structural comeback.
  putStrLn ""
  valueAttribution "B-M" 2007 2020 Concise
  valueAttribution "E-P" 2007 2020 Concise
  valueAttribution "CF-P" 2007 2020 Concise
  putStrLn ""

  valueAttribution "B-M" 2021 2025 Concise
  valueAttribution "E-P" 2021 2025 Concise
  valueAttribution "CF-P" 2021 2025 Concise
