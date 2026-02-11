{- |
Module      : Regression
Description : Functions for calculating factor regressions.

Maintainer  : Michael Dickens
Created     : 2026-02-10

-}

module Regression
  ( factorRegression
  , printFactorRegression
  , printFactorRegressionOrg
  , starsForPval
  ) where


import Quote
import Returns

import Data.List
import qualified Data.HashMap.Strict as Map
import Text.Printf


-- | Regress `retSeries - rf` over `factors`.
--
-- `rf` is the risk-free rate, which will be subtracted from `retSeries`. If the
-- risk-free rate should not be subtracted, pass in a 0 literal.
--
-- `factors` is a list of return series, each representing a factor.
--
-- If `retSeries` and `factors` cover different periods, then the regression
-- will only include periods where all necessary data is present. Periods with
-- partial data will be skipped.
--
-- Return a tuple of
-- - list including first the intercept, then the slope coefficients
-- - list of t-stats first for the intercept, then for the slope coefficients
-- - r^2 between predicted returns (according to the regression) and
--   actual returns
factorRegression :: RetSeries                    -- ^ Dependent variable
                 -> RetSeries                    -- ^ Risk-free rate
                 -> [RetSeries]                  -- ^ List of factors (independent variables)
                 -> ([Double], [Double], Double) -- ^ (coefficients, t-stats, r^2)
factorRegression retSeries rf factors =
  let -- Filter down to periods where retSeries, rf, and all factors have data
      periods = sort $ foldl (
        \accum factor -> intersect accum (Map.keys factor)
        ) (Map.keys $ retSeries + rf) factors

      -- Construct the independent variable from factor returns
      xs = map (\period -> map (\rets ->
                                  rets!period
                               ) factors) periods

      -- Construct the dependent variable from the return series
      ys = map (\period -> retSeries!period - rf!period) periods

      (intercept:coefs, tstats) = case multipleRegression xs ys of
        (i:c, ts) -> (i:c, ts)
        (wrong, _) -> error $ "multipleRegression failed, returning these coefs: "
                      ++ show wrong

      predictedReturns = map (sum . zipWith (*) coefs) xs
      corr = correlation (map (retSeries !) periods) predictedReturns

  in (intercept:coefs, tstats, corr**2)


-- | Provide a string that contains a number of asterisks based on the given
-- p-value. *, **, and *** indicate p < 0.05, p < 0.01, and p < 0.001
-- respectively.
starsForPval :: Double -> String
starsForPval pval
  | pval < 0.001 = "***"
  | pval < 0.01 = "**"
  | pval < 0.05 = "*"
  | otherwise = ""


-- | Print a factor regression as an equation.
--
-- >>> printFactorRegression myPortfolio rf [mkt - rf, smb, hml] ["Mkt", "SMB", "HML"]
-- 1.12*Mkt + 0.34*SMB + 0.93*HML
--	(r² = 0.96, alpha = -1.00%, ℒ = 6)

printFactorRegression :: RetSeries   -- ^ Dependent variable
                      -> RetSeries   -- ^ Risk-free rate
                      -> [RetSeries] -- ^ List of factors (independent variables)
                      -> [String]    -- ^ List of names of independent
                                     -- variables (for printing)
                      -> IO ()
printFactorRegression retSeries rf factors factorNames =
  let (intercept:coefs, tstat:_, rsqr) = factorRegression retSeries rf factors
      df = Map.size retSeries - length (intercept:coefs)
      pval = pValue tstat df
      likelihood = likelihoodRatio tstat df
      stars = starsForPval pval
  in do
    putStr
      $ foldl (
      \accum (name, coef) ->
        if accum == ""
        then accum ++ printf "%5.2f*%s" coef name
        else accum ++ printf " %s %.2f*%s"
             (if coef < 0 then "-" else "+" :: String) (abs coef) name
      )
      ("" :: String)
      $ zip factorNames coefs
    printf "\n\t(r² = %.2f, alpha = %5.2f%%%s, t-stat = %.2f, ℒ = %s)\n"
      (rsqr)
      (100 * ((1 + intercept)**12 - 1))
      stars
      tstat
      (prettyPrintLikelihood likelihood)


-- | Print a factor regression as a Markdown or org-mode table.
--
-- The final parameter is a list of names to match up with the provided list of
-- factors.
printFactorRegressionOrg :: RetSeries   -- ^ Dependent variable
                         -> RetSeries   -- ^ Risk-free rate
                         -> [RetSeries] -- ^ List of factors (independent variables)
                         -> [String]    -- ^ List of names of independent variables
                                        -- (for printing)
                         -> IO ()
printFactorRegressionOrg retSeries rf factors factorNames = do
  let (coefs, tstats, rsqr) = factorRegression retSeries rf factors
  let df = Map.size retSeries - length factors
  putStrLn "|                  |       | t-stat | likelihood |"
  putStrLn "|------------------|-------|--------|------------|"
  let alphaName = "annual alpha"
  sequence $ for (zip3 (alphaName:factorNames) coefs tstats) $ \(name, coef', tstat) -> do
    let likelihood = likelihoodRatio tstat df
    let pval = pValue tstat df
    let stars = starsForPval pval
    let coef = if name == alphaName then 100 * ((1 + coef')**12 - 1) else coef'
    let percentSign = if name == alphaName then "%" else "" :: String
    printf "| %-16s | %5.2f%s%-3s | %.1f | %s |\n"
      name coef percentSign stars tstat (prettyPrintLikelihood likelihood)
  printf   "| r²               | %.2f  |        |            |\n" rsqr
