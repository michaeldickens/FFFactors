{- |
Module      : MVO
Description : Quasi-mean-variance optimization where the risk measure
              does not have to be variance.
Maintainer  : Michael Dickens
Created     : 2026-01-28

Originally written by Claude Opus 4.5.

The NLOPT library must be installed on your system:
  Ubuntu/Debian: apt-get install libnlopt-dev
  macOS: brew install nlopt
-}

module MVO
    ( optimizePortfolio
    , printMVO
    , MVOConfig(..)
    , mvoDefaults
    , mvoFactorConfig
    ) where

import qualified Data.HashMap.Strict as Map

import Numeric.LinearAlgebra (Vector, fromList, toList)
import Numeric.NLOPT
import Text.Printf

import French
import Returns hiding (toList)
import qualified Returns as R


-- | Optimization configuration
data MVOConfig = MVOConfig
    { riskMetric      :: [Double] -> Double -- ^ A function from a return series
                                            -- to a number quantifying risk.
                                            -- Higher is worse.
    , maxRiskPct      :: Double   -- ^ Maximum allowed risk (pct)
    , maxLeverage     :: Double   -- ^ Maximum sum of |weights|
    , allowShorts     :: Bool     -- ^ Allow negative weights
    , leverageCostPct :: Double   -- ^ Annual cost of leverage/shorts over RF
    , excessOfRF      :: Bool     -- ^ If True, treat the provided return series
                                  -- as giving excess returns. If False, treat
                                  -- them as absolute returns. Either way, the
                                  -- returned portfolio will be in terms of
                                  -- absolute returns.
    }


-- | Sensible defaults for MVOConfig.
mvoDefaults :: MVOConfig
mvoDefaults =
  MVOConfig { riskMetric      = ulcerIndex -- ^ Ulcer index is the most
                                           -- underrated measure of risk and
                                           -- everyone should use it.
            , maxRiskPct      = 10         -- ^ An ulcer index of 10 roughly
                                           -- matches 60/40, or equities if you
                                           -- exclude the Great Depression.
            , maxLeverage     = 1.0
            , allowShorts     = False
            , leverageCostPct = 0
            , excessOfRF      = False
            }


mvoFactorConfig :: MVOConfig
mvoFactorConfig =
  MVOConfig { riskMetric      = ulcerIndex
            , maxRiskPct      = 10
            , maxLeverage     = 99
            , allowShorts     = True
            , leverageCostPct = 0
            , excessOfRF      = True
            }


-- | Determine whether an NLOPT `Result` indicates success.
--
-- Note: This function is defined by Numeric.NLOPT but it's not exposed
-- externally so I copy/pasted it here.
isSuccess :: Result -> Bool
isSuccess SUCCESS         = True
isSuccess STOPVAL_REACHED = True
isSuccess FTOL_REACHED    = True
isSuccess XTOL_REACHED    = True
isSuccess MAXEVAL_REACHED = True
isSuccess MAXTIME_REACHED = True
isSuccess _               = False


-- | Combine asset returns into portfolio returns. The provided returns should be in excess of the risk-free rate; the combined portfolio will include the risk-free rate.
--
-- An investor must pay `leverageCost` per year for short borrowing as well as
-- for any net leverage (longs minus shorts).
combineReturns :: [Double]
               -> [RetSeries]
               -> RetSeries
               -> Double
               -> RetSeries
combineReturns weights histories rf leverageCost =
  let netLong = sum weights
      totalShort = negate $ sum $ map (min 0) weights
      extraCost = leverageCost * (max 0 (netLong - 1) + totalShort)
      costPerPeriod = (1 + extraCost)**(1/12) - 1
  in Map.map (\x -> x - costPerPeriod)
     $ rf + (sum $ zipWith (\w -> Map.map (* w)) weights histories)


-- | Smoothen a return series by forcing all values <= -1 to be between -1 and
-- -0.99 instead, while preserving monotonicity. This allows CAGR and ulcer
-- index to be well-defined on any return series.
--
-- This prevents `optimizePortfolio` from getting stuck when it runs into a
-- portfolio with -100% CAGR.
smoothen :: RetSeries -> [Double]
smoothen history = flip map (R.toList history) $ \x ->
  if x >= -0.99
  then x
  -- tanh (x + 0.99) produces output between -1 and 0 when x < -0.99
  else tanh (x + 0.99) / 100 - 0.99


-- | Maximize annualized return subject to ulcer index and leverage constraints.
--
-- Return a tuple of (Result, weights, optimal portfolio)
optimizePortfolio
    :: MVOConfig                     -- ^ Configuration
    -> [RetSeries]                   -- ^ Return series for each asset
    -> RetSeries                     -- ^ Risk-free rate
    -> (Result, [Double], RetSeries) -- ^ (`NLOpt` `Result` object, list of
                                     -- weights, return series for the optimal
                                     -- portfolio)
optimizePortfolio cfg histories' rf
  -- TODO: There should be a better way to impose costs. A long/short asset
  -- should have equal cost whether it's long or short, but right now
  -- `imposeCost` makes the cost *increase* the return if the asset is shorted.
  | periodsUnion /= periods =
    error "optimizePortfolio failed: return series have inconsistent date ranges"
  | otherwise =
    let n = length histories
        maxRisk = maxRiskPct cfg / 100
        leverageCost = leverageCostPct cfg / 100
        makePortfolio wts = combineReturns wts histories rf leverageCost

        -- Objective: minimize negative return (= maximize return)
        objective :: Vector Double -> Double
        objective w = -(annualizedReturn $ smoothen $ makePortfolio (toList w))

        -- Risk constraint: riskMetric(w) - maxRisk <= 0
        riskConstraint :: InequalityConstraint ScalarConstraint VectorConstraint
        riskConstraint = InequalityConstraint
            { ineqConstraintFunctions = Scalar $ \w ->
                (riskMetric cfg $ smoothen $ makePortfolio (toList w)) - maxRisk
            , ineqConstraintTolerance = 1e-6
            }

        -- Leverage constraint: sum(|w|) - maxLeverage <= 0
        leverageConstraint :: InequalityConstraint ScalarConstraint VectorConstraint
        leverageConstraint = InequalityConstraint
            { ineqConstraintFunctions = Scalar $ \w ->
                sum (map abs $ toList w) - maxLeverage cfg
            , ineqConstraintTolerance = 1e-6
            }

        -- Bounds
        lowerBound = LowerBounds $ fromList $ replicate n $
            if allowShorts cfg then -(maxLeverage cfg) else 0
        upperBound = UpperBounds $ fromList $ replicate n (maxLeverage cfg)

        -- Stopping criteria
        stop = ObjectiveRelativeTolerance 1e-6 :| [MaximumEvaluations 10000]

        -- Initial guess is an equal-weighted portfolio. An initial guess must
        -- be supplied or else sometimes the optimizer will fail to converge.
        initialGuess = InitialStep $ fromList $ replicate n (1 / fromIntegral n)

        -- COBYLA: derivative-free local optimizer with nonlinear constraint support
        algorithm = COBYLA objective [lowerBound, upperBound]
                    [riskConstraint, leverageConstraint]
                    []       -- no equality constraints
                    (Just initialGuess)

        problem = LocalProblem (fromIntegral n) stop algorithm

        -- Initial guess: equal weights
        x0 = fromList $ replicate n (maxLeverage cfg / fromIntegral n)

        solution = minimizeLocal' problem x0

        weights = toList $ solutionParams solution

        portfolio = makePortfolio weights

    in (solutionResult solution, weights, portfolio)

    -- in case minimizeLocal problem x0 of
        -- Right (Solution _ params _) -> Right (toList params)
        -- Left err -> Left $ "Optimization failed: " ++ show err

  where periodsUnion = getDateRange $ foldl1 Map.union histories
        periods = getDateRange $ foldl1 Map.intersection (rf:histories)
        histories = if excessOfRF cfg
                    then histories'
                    else map (\h -> h - rf) histories'


-- | Run `optimizePortfolio` and print the result.
--
-- Note that printMVO does not require passing in an `rf` parameter; instead, it
-- loads `rf` from file.
printMVO :: MVOConfig     -- ^ Configuration
         -> [RetSeries]   -- ^ Return series for each asset
         -> [String]      -- ^ Name of each asset (for printing)
         -> IO (Result, [Double], RetSeries)  -- ^ (`NLOpt` `Result` object,
                                              -- list of weights, return series
                                              -- for the optimal portfolio
printMVO cfg histories names = do
  rf <- loadRF
  let (result, weights, portfolio) = optimizePortfolio cfg histories rf
  handle result weights rf
  return (result, weights, portfolio)

  where handle result weights rf
          | isSuccess result = do
              putStrLn "\n--- Optimization Succeeded ---\n"
              printWeights weights rf
          | result == ROUNDOFF_LIMITED = do
              putStrLn "\n--- Optimization Partial Failure ---\n"
              putStrLn "Warning: Optimization halted because roundoff errors limited progress. Result may still be usable.\n"
              printWeights weights rf
          | otherwise = do
              printf "\nOptimization failed: %s\n\n" $ show result

        printWeights weights rf = do
          let leverageCost = leverageCostPct cfg / 100
          let portfolio = combineReturns weights histories rf leverageCost
          printStats' portfolio
          printf "Optimal Weights: (total notional = %.2fx)\n"
            (sum $ map abs weights)
          sequence $ for (zip weights names) $ \(wt, name) -> do
            printf "    %-20s: %6.1f%%\n" name (100 * wt)
          putStrLn ""
