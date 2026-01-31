{- |
Module      : Main
Description :

Maintainer  : Michael Dickens <mdickens93@gmail.com>
Created     : 2018-10-12

-}

module Main where

import ChartPlot
import FrenchQuote
import French
import MVO

import Drawdown
import MaybeArithmetic
import Returns
import Tools

import Control.Monad
import Data.Foldable (for_)
import Data.Function (on)
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace
import Numeric.LinearAlgebra hiding ((!), toList)
import Numeric.LinearAlgebra.Data hiding ((!), toList)
import qualified Statistics.Distribution as Dist
import qualified Statistics.Distribution.StudentT as StudentT
import Statistics.Regression
import System.Environment
import System.IO
import Text.Printf


-- | Equal-weight all segments in a given list such that the weights sum to 1.
equally :: [a] -> [(a, Double)]
equally xs = zip xs $ repeat (1 / fromIntegral (length xs))


printIndustryReturns :: IO ()
printIndustryReturns = do
  usMarket <- loadDB "French/3_Factors_US.csv"
  industryMap <- loadDB "French/Industry_Portfolios_49.csv"
  let marketRets = getRets [("Mkt-RF", 1), ("RF", 1)] usMarket
  let industrySegments = map Text.unpack $ Map.keys $ head $ Map.elems industryMap
  let industryRets = map (\s -> getRets1 s industryMap) industrySegments
  let weights = map (\s -> (s, 1/49)) industrySegments
  let equalWtRets = getRets weights industryMap
  let industryLifetimeRets = map (\r -> 100 * ((1 + geometricMean (mapToOrderedList r))**12 - 1)) industryRets
  let pairs = sortBy (compare `on` snd) $ zip industrySegments industryLifetimeRets
  for_ pairs $ \(s, r) -> printf "%s: %.1f\n" s r


globalMom :: IO ()
globalMom = do
  usQuotes <- loadDB "French/3_Factors_US.csv"
  exQuotes <- loadDB "French/3_Factors_Developed_ex_US.csv"
  euQuotes <- loadDB "French/3_Factors_Europe.csv"
  jpQuotes <- loadDB "French/3_Factors_Japan.csv"
  asQuotes <- loadDB "French/3_Factors_Asia_Pacific_ex_Japan.csv"
  emQuotes <- loadDB "French/5_Factors_Emerging.csv"
  coQuotes <- loadDB "AQR/Commodities.csv"
  bdQuotes <- loadDB "10Y_Treasury_Returns.csv"

  let allRets = Map.fromList [
        ("us", getRets [("Mkt-RF", 1), ("RF", 1)] usQuotes),
        -- ("ex", getRets [("Mkt-RF", 1), ("RF", 1)] exQuotes),
        ("eu", getRets [("Mkt-RF", 1), ("RF", 1)] euQuotes),
        -- ("jp", getRets [("Mkt-RF", 1), ("RF", 1)] jpQuotes),
        -- ("as", getRets [("Mkt-RF", 1), ("RF", 1)] asQuotes),
        ("em", getRets [("Mkt-RF", 1), ("RF", 1)] emQuotes),
        -- ("co", getRets [("Excess return of equal-weight commodities portfolio", 1), ("Excess return of long/short commodities portfolio", 1/3), ("RF", 1)] coQuotes),
        ("bd", getRets [("Return M", 1)] bdQuotes)
        ]

  let gemReturns :: Int -> IO (RetSeries)
      gemReturns lookbackMonths = do
        -- let dateRange = jointDateRange [usQuotes, exQuotes, euQuotes, jpQuotes, asQuotes, emQuotes, coQuotes, bdQuotes]
        let dateRange :: [Period]
            dateRange = jointDateRange $ Map.elems allRets
        let dateRangeAfterOneYear = drop (max 12 lookbackMonths) dateRange
        let rf = getRets1 "RF" usQuotes

        let trendRets = Map.map (\m -> trendOverlay TMOM lookbackMonths rf m m) allRets

        let gem :: Map.HashMap String (RetSeries) -> Int ->
                   RetSeries
            gem retsBase numSegmentsToHold = Map.fromList $ zip dateRange $ map
              (\period -> let topSegments = take numSegmentsToHold $ reverse
                                $ sortBy
                                (compare `on`
                                  (\segment -> fromJust $ tsMomentum lookbackMonths period (allRets Map.! segment)
                                  )) $ Map.keys allRets
                          in average $ map (\segment -> retsBase Map.! segment Map.! period) topSegments
              ) dateRangeAfterOneYear

        let gemNoTrend = gem allRets
        let gemTrend = gem trendRets
        let raaNoTrend = Map.fromList $ map (\p -> (p, average $ map (\r -> r Map.! p) $ Map.elems allRets)) dateRangeAfterOneYear
        let raaTrend = Map.fromList $ map (\p -> (p, average $ map (\r -> r Map.! p) $ Map.elems trendRets)) dateRangeAfterOneYear

        return $ gemTrend 1

  gemReturns 12 >>= printStatsOrg "12-month" . startingPeriod 2015 1


-- | Given a map of breakpoints for some fundamental-to-price ratio, approximate
-- the average valuation for stocks that fall within the given percentile range.
-- For example, for a percentile range of (70, 100), find the approximate
-- average fundamental-to-price for the cheapest 30% of stocks.
--
-- The exact correct value cannot be computed without individual stock data, so
-- this function uses the trapezoid rule to approximate the answer.
getAverageFromBreakpoints :: FrenchQuoteMap -> (Int, Int) -> RetSeries
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


data PrintType = Concise | Verbose | AllRets | Drawdowns deriving (Eq)


-- | Value factor valueAttribution broken out into multiple expansion + structural
-- return.
valueAttribution :: String -> Int -> Int -> PrintType -> IO ()
valueAttribution metric startYear endYear printType = do
  quotes <- loadDB $ (printf "French/Portfolios_%s_Value_Wt.csv" metric :: String)
  breakpoints <- loadDB $ (printf "French/Breakpoints_%s.csv" metric :: String)

  -- To match RAFI paper and French breakpoints, years should end in June
  let endMonth = 6

  let normalize x = 100 * (log x) / (fromIntegral $ endYear - startYear)

  let revaluation breakpoints =
        log $ (breakpoints ! Period startYear 1) / (breakpoints ! Period endYear 1)

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
        let monthlyPrices = zip (sort $ Map.keys rets) (returnsToPrices $ mapToOrderedList rets)
        in Map.fromList $ map (\(p, r) -> (year p, r)) $ sortBy (compare `on` fst)
            $ filter (\(p, _) -> month p == endMonth) monthlyPrices

  let growthMultiples = getMultiples (0, 30)
  let valueMultiples = getMultiples (70, 100)
  let growthPrices = retsToYearlyPrices $ getRets1 "Lo 30" quotes
  let valuePrices = retsToYearlyPrices $ getRets1 "Hi 30" quotes

  let hmlMultiples' = Map.intersectionWith (/) growthMultiples valueMultiples
  let hmlMultiples = Map.filterWithKey (\k _ -> k >= startYear && k <= endYear) hmlMultiples'

  -- don't calculate using `getRets` functionality because that will rebalance
  -- monthly, but we want annual rebalancing
  let hmlPrices = Map.intersectionWith (/) valuePrices growthPrices

  let getFundamentals prices multiples = Map.intersectionWith (/) prices multiples

  -- Technically, these are adjusted for dividends
  let growthFundamentals = getFundamentals growthPrices growthMultiples
  let valueFundamentals = getFundamentals valuePrices valueMultiples
  let hmlFundamentals = Map.intersectionWith (/) valueFundamentals growthFundamentals

  when (printType == Verbose) $
    printf "=== Value Factor Attribution (%s), %d to %d ===\n\n" metric startYear endYear

  when (printType == Concise) $ do
    printf "%d to %d: " startYear endYear
    printf "%4.1f%% return = %4.1f%% valuation + %4.1f%% structural\n"
      (normalize $ (hmlPrices ! endYear) / (hmlPrices ! startYear))
      (negate $ normalize $ (hmlMultiples ! endYear) / (hmlMultiples ! startYear))
      (normalize $ (hmlFundamentals ! endYear) / (hmlFundamentals ! startYear))

  let logRets = zipWith (\x y -> log (y / x)) (mapToOrderedList hmlPrices) (tail $ mapToOrderedList hmlPrices)
  let logMultipleChange = zipWith (\x y -> log (y / x)) (mapToOrderedList hmlMultiples) (tail $ mapToOrderedList hmlMultiples)
  let logStructural =
        zipWith (\x y -> log (x / y)) (mapToOrderedList hmlFundamentals) (tail $ mapToOrderedList hmlFundamentals)

  -- print regression of 10-year returns on valuation spreads
  when (printType == Verbose && (endYear - 10 > startYear)) $ do
    let forwardRets = map (\y -> (hmlPrices!(y + 10) / hmlPrices!y)**(1/10) - 1) [startYear..(endYear - 10)]
    putStr "\tregression of 10-year returns on starting valuation spreads: \n\t\t"
    printLinearRegression (map (hmlMultiples!) [startYear..(endYear-10)]) forwardRets

  -- print each of the 3 return series
  when (printType == AllRets) $ do
    for_ [startYear..endYear] $ \y -> do
      printf "%d\t%.3f\t%.3f\t%.3f\n" y (hmlPrices!y / hmlPrices!startYear) (1 / (hmlMultiples!y / hmlMultiples!startYear)) (hmlFundamentals!y / hmlFundamentals!startYear)

  -- print drawdowns
  when (printType == Drawdowns) $ do
    let priceDD = rollingDrawdowns $ pricesToReturns hmlPrices
    let multipleDD = rollingDrawdowns $ pricesToReturns $ Map.map (1 /) hmlMultiples
    let fundDD = rollingDrawdowns $ pricesToReturns hmlFundamentals
    for_ (zip4 [startYear..endYear] priceDD multipleDD fundDD) $ \(y, price, mult, fund) -> do
      printf "%d\t%.3f\t%.3f\t%.3f\n" y price mult fund

  putStr "Max value spread: "
  print $ maximumBy (compare `on` snd) $ Map.toList hmlFundamentals


main = do
  gm <- retsFromFile1 "Barclay_Global_Macro_Index.csv" "Global Macro Index"

  let gmScary = growUlcer 2 gm

  printStatsOrg "GM" gm
  printStatsOrg "GM scary" $ gmScary

  print $ take 5 $ mapToOrderedList gm
  print $ take 5 $ mapToOrderedList gmScary

  plotLineGraph "output.png" "Drawdown guys"
    "Drawdowns"
    (take 120 $ sort $ Map.keys gm)
    [ ("GM", rollingDrawdowns gm)
    , ("GM scary", rollingDrawdowns gmScary)
    ]

  -- putStrLn $ intercalate "\n" $ zipWith (\x y -> show x ++ "\t" ++ show y) (rollingDrawdowns gm) (rollingDrawdowns gmScary)
