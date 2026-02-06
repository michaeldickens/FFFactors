"""
Ulcer Performance Index (UPI) Portfolio Optimizer

This script finds the asset combination that maximizes the Ulcer Performance Index,
which measures risk-adjusted returns using drawdown-based risk (Ulcer Index) rather
than standard deviation.

UPI = (Total Return - Risk-free Return) / Ulcer Index

where Ulcer Index = sqrt(mean(squared percentage drawdowns from prior peaks))

The optimization problem is non-convex due to the path-dependent nature of drawdowns,
so we use multiple approaches:
1. Sequential Least Squares Programming (SLSQP) with numerical gradients
2. Basin-hopping global optimization
3. Grid search for small portfolios
4. Differential evolution for robustness

Originally written by Claude Opus 4.5; modifications by Michael Dickens
"""

import numpy as np
import pandas as pd
from scipy.optimize import minimize, differential_evolution, basinhopping
from typing import Optional, Tuple, List, Dict, Callable, Union
from dataclasses import dataclass
from pathlib import Path
import warnings


@dataclass
class OptimizationResult:
    """Container for optimization results."""

    weights: np.ndarray
    portfolio_prices: np.ndarray
    periods_per_year: int
    utility: float
    success: bool
    message: str


def returns_to_prices(returns: np.ndarray):
    # Convert returns to cumulative prices
    # Price[t] = Price[t-1] * (1 + return[t])
    cumulative = np.cumprod(1 + returns, axis=0)
    # Prepend initial price of 1.0
    if len(returns.shape) > 1:
        prices = np.vstack([np.ones(returns.shape[1]), cumulative])
    else:
        prices = np.concatenate(([1], cumulative))
    return prices


def read_returns_csv(
    filepath: Union[str, Path],
    asset_columns: Optional[List[str]] = None,
    rf_column: Optional[str] = "RF",
    date_column: Optional[str] = None,
    returns_in_percent: bool = True,
    convert_to_prices: bool = True,
) -> Tuple[np.ndarray, List[str], Optional[np.ndarray]]:
    """
    Read return series from a CSV file (e.g., Fama-French format).

    Parameters
    ----------
    filepath : str or Path
        Path to the CSV file
    asset_columns : list of str, optional
        Column names to use as assets. If None, uses all columns except
        the date column and risk-free rate column.
    rf_column : str, optional
        Column name for risk-free rate. Set to None if not present.
        Default is 'RF'.
    date_column : str, optional
        Column name for dates. If None, assumes first column is the index.
    returns_in_percent : bool
        If True, returns are in percent (e.g., 5.0 means 5%).
        If False, returns are in decimal (e.g., 0.05 means 5%).
    convert_to_prices : bool
        If True, converts returns to cumulative price series starting at 1.
        If False, returns the return series directly.

    Returns
    -------
    tuple of (data, asset_names, risk_free_rates)
        - data: np.ndarray of shape (n_periods, n_assets) containing either
          prices (if convert_to_prices=True) or returns
        - asset_names: list of asset column names
        - risk_free_rates: np.ndarray of risk-free rates per period (or None)

    Examples
    --------
    >>> # Read Fama-French factors
    >>> prices, names, rf = read_returns_csv('F-F_Research_Data_Factors.csv')
    >>> print(names)
    ['Mkt-RF', 'SMB', 'HML', 'RMW', 'CMA']

    >>> # Then optimize
    >>> result = optimize_portfolio(prices, risk_free_rate=rf.mean() * 12)
    """
    # Read CSV
    df = pd.read_csv(filepath, index_col=0 if date_column is None else None)

    if date_column is not None:
        df = df.set_index(date_column)

    # Identify asset columns
    if asset_columns is None:
        # Use all columns except RF
        exclude_cols = set()
        if rf_column is not None and rf_column in df.columns:
            exclude_cols.add(rf_column)
        asset_columns = [col for col in df.columns if col not in exclude_cols]

    # Extract asset returns
    returns_df = df[asset_columns].copy()

    # Extract risk-free rate if present
    rf_rates = None
    if rf_column is not None and rf_column in df.columns:
        rf_rates = df[rf_column].values
        if returns_in_percent:
            rf_rates = rf_rates / 100.0

    # Convert returns from percent to decimal if needed
    returns = returns_df.values
    if returns_in_percent:
        returns = returns / 100.0

    if convert_to_prices:
        return returns_to_prices(returns), asset_columns, rf_rates
    else:
        return returns, asset_columns, rf_rates


def compute_utility(wealth: float, rra: float) -> float:
    if rra == 1:
        return np.log(wealth)
    return (wealth ** (1 - rra) - 1) / (1 - rra)


def compute_utility_fast(wealth: float, rra: float) -> float:
    return wealth ** (1 - rra) / (1 - rra)


def compute_portfolio_prices(
    prices: np.ndarray, weights: np.ndarray, risk_free_rate: np.ndarray = None
) -> np.ndarray:
    """
    Compute portfolio value series from price series and weights. Prices
    must be net of the risk-free rate.
    """
    returns = prices[1:] / prices[:-1] - 1
    portfolio_returns = returns @ weights
    portfolio_prices = returns_to_prices(portfolio_returns)

    # Price cannot go below 0. Allowing negative prices can create math issues
    portfolio_prices = np.clip(portfolio_prices, 0, None)

    # Leverage requires paying RF; shorts pay back RF. If weights sum to 0, the
    # whole portfolio is invested in risk-free T-bills.
    if risk_free_rate is not None:
        # TODO: add a way to set a borrowing cost above RF
        #
        # TODO: The way to incorporate RF depends on whether the prices are
        # already net of RF (e.g. are they Mkt-RF or Mkt?). There should be an
        # input parameter to specify. or maybe make the caller deal with it.
        if False:
            # do it this way if the inputs are NOT net of RF
            scaled_rf = risk_free_rate * (1 - np.sum(weights))
        else:
            scaled_rf = risk_free_rate

        # Note: It would incorrect to convert RF into prices and then multiply
        # the prices because the weights would not be consistent.
        portfolio_returns = portfolio_prices[1:] / portfolio_prices[:-1] - 1
        portfolio_returns += scaled_rf
        portfolio_prices = returns_to_prices(portfolio_returns)

    return portfolio_prices


def compute_ulcer_index(prices: np.ndarray) -> float:
    """
    Compute the Ulcer Index (as a percentage) for a price series.

    The Ulcer Index measures the depth and duration of drawdowns from prior peaks.
    UI = sqrt(mean(squared_percentage_drawdowns))
    """
    running_max = np.maximum.accumulate(prices)
    drawdowns = 100 * (prices / running_max - 1)
    sum_sq = np.sum(drawdowns**2)
    ui = np.sqrt(sum_sq / len(prices))

    return ui


def compute_annualized_return(prices: np.ndarray, periods_per_year: int = 12) -> float:
    """
    Compute annualized return (as a percentage) from a price series.
    """
    total_return = prices[-1] / prices[0]
    n_periods = len(prices) - 1
    years = n_periods / periods_per_year

    if years <= 0:
        return 0.0

    annualized = (total_return ** (1 / years) - 1) * 100
    return annualized


def compute_stdev(prices: np.ndarray, periods_per_year: int = 12) -> float:
    """
    Compute annual standard deviation (as a percentage) from a price series.
    """
    returns = prices[1:] / prices[:-1] - 1
    return np.std(returns) * np.sqrt(periods_per_year) * 100


def compute_portfolio_utility(
    prices: np.ndarray,
    weights: np.ndarray,
    risk_free_rate: np.ndarray,
    periods_per_year: int = 12,
    debug: bool = False,
) -> float:
    rra = 2
    time_horizons = periods_per_year * np.array([5, 10, 20, 40])

    if any(abs(weights) >= 100):
        print(
            f"Warning: Are you sure you meant for the weights to be {weights}? Those are not percentages."
        )

    portfolio_prices = compute_portfolio_prices(prices, weights, risk_free_rate)
    if rra >= 1 and any(portfolio_prices == 0):
        # Returning early isn't strictly necessary, but it prevents
        # divide-by-zero warnings
        return -np.inf

    portfolio_returns = portfolio_prices[1:] / portfolio_prices[:-1] - 1
    r = np.mean(portfolio_returns)
    running_max = np.maximum.accumulate(portfolio_prices)
    drawdowns = 1 - portfolio_prices / running_max

    # probability of having to withdraw is proportional to the size of the
    # drawdown
    # p_withdraw = drawdowns
    p_withdraw = np.ones(len(drawdowns))

    if False:
        # this version is more efficient but empirically it doesn't seem to
        # matter much. might matter if I were using more time horizons
        dd_utilities = compute_utility_fast(1 - drawdowns, rra)
        dd_component = np.dot(dd_utilities, p_withdraw)
        returns_component = (1 + r) ** ((1 - rra) * time_horizons)
        utilities = returns_component * dd_component

    else:
        utilities = []
        for t in time_horizons:
            prices_with_dd = (1 + r) ** t * (1 - drawdowns)
            dd_utilities = compute_utility(prices_with_dd, rra)
            utilities.append(np.dot(dd_utilities, p_withdraw))

    return np.mean(utilities)


def compute_upi_for_real(
    prices: np.ndarray,
    weights: np.ndarray,
    risk_free_rate: np.ndarray,
    periods_per_year: int = 12,
) -> float:
    """
    Compute the Ulcer Performance Index for a portfolio.

    UPI = (Annualized Return - Risk-free Rate) / Ulcer Index
    """
    portfolio_prices = compute_portfolio_prices(prices, weights, risk_free_rate)

    ui = compute_ulcer_index(portfolio_prices)
    ann_return = compute_annualized_return(portfolio_prices, periods_per_year)

    excess_return = ann_return - risk_free_rate

    # Handle edge case where UI is very small
    if ui < 1e-10:
        return np.inf if excess_return > 0 else -np.inf if excess_return < 0 else 0

    return excess_return / ui


def _negative_utility(
    weights: np.ndarray,
    prices: np.ndarray,
    risk_free_rate: float,
    periods_per_year: int,
) -> float:
    """Negative UPI for minimization."""
    utility = compute_portfolio_utility(
        prices, weights, risk_free_rate, periods_per_year
    )
    # Handle infinities
    if np.isinf(utility):
        return 1e10 if upi < 0 else -1e10
    return -utility


def optimize_portfolio(
    prices: np.ndarray,
    risk_free_rate: np.ndarray,
    periods_per_year: int,
    allow_short: bool = False,
    max_leverage: float = 1.0,
    min_weight: Optional[float] = None,
    max_weight: Optional[float] = None,
    initial_weights: Optional[np.ndarray] = None,
    n_restarts: int = 10,
) -> OptimizationResult:
    """
    Optimize portfolio weights to maximize UPI using SLSQP.

    SLSQP uses numerical gradients since UPI is not analytically differentiable
    due to the path-dependent nature of drawdowns.
    """
    n_assets = prices.shape[1]

    # Set default bounds
    if min_weight is None:
        min_weight = -max_leverage if allow_short else 0
    if max_weight is None:
        max_weight = max_leverage

    bounds = [(min_weight, max_weight) for _ in range(n_assets)]

    # Constraints
    constraints = []

    # Sum of weights = 1 (fully invested)
    # TODO: this forces the weights to sum to 1 even if leverage is allowed
    # constraints.append({
    # 'type': 'eq',
    # 'fun': lambda w: np.sum(w) - 1
    # })

    # Leverage constraint: sum of |weights| <= max_leverage
    constraints.append(
        {"type": "ineq", "fun": lambda w: max_leverage - np.sum(np.abs(w))}
    )

    best_result = None
    best_utility = -np.inf

    for i in range(n_restarts):
        # Generate starting point
        if i == 0 and initial_weights is not None:
            x0 = initial_weights
        elif i == 0:
            # Equal weight as first starting point
            x0 = np.ones(n_assets) / n_assets
        else:
            # Random starting point
            if allow_short:
                x0 = np.random.randn(n_assets)
                x0 = x0 / np.sum(np.abs(x0)) * np.random.uniform(0.5, max_leverage)
                x0 = x0 / np.sum(x0)  # Ensure sum = 1
            else:
                x0 = np.random.dirichlet(np.ones(n_assets))

        try:
            result = minimize(
                _negative_utility,
                x0,
                args=(prices, risk_free_rate, periods_per_year),
                method="SLSQP",
                bounds=bounds,
                constraints=constraints,
                options={"maxiter": 1000, "ftol": 1e-9},
            )

            if result.success or result.fun < -best_utility:
                utility = -result.fun
                if utility > best_utility:
                    best_utility = utility
                    best_result = result
        except Exception:
            continue

    if best_result is None:
        return OptimizationResult(
            weights=np.ones(weights),
            portfolio_prices=np.zeroes(prices),
            periods_per_year=periods_per_year,
            utility=-np.inf,
            success=False,
            message="Optimization failed",
        )

    weights = best_result.x
    portfolio_prices = compute_portfolio_prices(prices, weights, risk_free_rate)
    ui = compute_ulcer_index(portfolio_prices)
    ann_return = compute_annualized_return(portfolio_prices, periods_per_year)
    stdev = compute_stdev(portfolio_prices, periods_per_year)

    return OptimizationResult(
        weights=weights,
        portfolio_prices=portfolio_prices,
        periods_per_year=periods_per_year,
        utility=best_utility,
        success=best_result.success,
        message=best_result.message,
    )


def compute_efficient_frontier(
    target_ui: float,
    prices: np.ndarray,
    risk_free_rate: np.ndarray,
    periods_per_year: int = 12,
    n_points: int = 50,
    allow_short: bool = False,
    max_leverage: float = 1.0,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Compute the UPI-based efficient frontier.

    Find the portfolio that maximizes return for the given level of Ulcer Index risk.

    Returns
    -------
    tuple of (ulcer_indices, returns, weights_list)
    """
    n_assets = prices.shape[1]

    # First, find the range of achievable Ulcer Index values
    min_ui = np.inf
    max_ui = -np.inf

    # Check individual assets
    for i in range(n_assets):
        weights = np.zeros(n_assets)
        weights[i] = 1.0
        portfolio_prices = compute_portfolio_prices(prices, weights, risk_free_rate)
        ui = compute_ulcer_index(portfolio_prices)
        min_ui = min(min_ui, ui)
        max_ui = max(max_ui, ui)

    # Also check equal weight
    eq_weights = np.ones(n_assets) / n_assets
    eq_values = compute_portfolio_prices(prices, eq_weights, risk_free_rate)
    eq_ui = compute_ulcer_index(eq_values)
    min_ui = min(min_ui, eq_ui)
    max_ui = max(max_ui, eq_ui)

    frontier_uis = []
    frontier_returns = []
    frontier_weights = []

    # Maximize return subject to UI <= target
    def objective(weights):
        portfolio_prices = compute_portfolio_prices(prices, weights, risk_free_rate)
        ann_return = compute_annualized_return(portfolio_prices, periods_per_year)
        return -ann_return

    def ui_constraint(weights):
        portfolio_prices = compute_portfolio_prices(prices, weights, risk_free_rate)
        ui = compute_ulcer_index(portfolio_prices)
        return target_ui - ui

    bounds = [
        (0 if not allow_short else -max_leverage, max_leverage)
        for _ in range(n_assets)
    ]

    constraints = [
        {"type": "ineq", "fun": lambda w: max_leverage - np.sum(np.abs(w))},
        {"type": "ineq", "fun": ui_constraint},
    ]

    x0 = np.ones(n_assets) / n_assets

    result = minimize(
        objective,
        x0,
        method="SLSQP",
        bounds=bounds,
        constraints=constraints,
        options={"maxiter": 500},
    )

    if result.success:
        weights = result.x
        portfolio_prices = compute_portfolio_prices(prices, weights, risk_free_rate)
        ui = compute_ulcer_index(portfolio_prices)
        ann_return = compute_annualized_return(
            portfolio_prices, periods_per_year
        )

        return (ui, ann_return, weights)

    return (None, None, None)


def print_result(result: OptimizationResult):
    prices = result.portfolio_prices
    returns = prices[1:] / prices[:-1] - 1

    print(f"Success: {result.success}")
    print(f"Utility: {result.utility:.4f}")
    print(f"Mean : {100 * periods_per_year * np.mean(returns):.2f}%")
    print(f"CAGR : {compute_annualized_return(prices, result.periods_per_year):.2f}%")
    print(f"Stdev: {compute_stdev(prices, result.periods_per_year):.2f}%")
    print(f"Ulcer: {compute_ulcer_index(prices):.4f}%")
    print(f"Optimal Weights: (total = {sum(result.weights):.2f}x)")
    for name, weight in zip(asset_names, result.weights):
        print(f"  {name}: {weight*100:.1f}%")
    print()


if __name__ == "__main__":
    import sys
    import os

    if len(sys.argv) >= 2:
        csv_file = sys.argv[1]
    else:
        csv_file = "resources/French/5_Factors.csv"

    print("=" * 70)
    print("ULCER PERFORMANCE INDEX (UPI) PORTFOLIO OPTIMIZER")
    print("=" * 70)
    print()
    print(f"Loading data from: {csv_file}")
    print()

    # Read the CSV file
    try:
        prices, asset_names, rf = read_returns_csv(
            csv_file, rf_column="RF", returns_in_percent=True, convert_to_prices=True
        )
    except FileNotFoundError:
        print(f"Error: File '{csv_file}' not found.")
        print("Usage: python upi_optimizer.py [path/to/returns.csv]")
        sys.exit(1)
    except Exception as e:
        print(f"Error reading file: {e}")
        sys.exit(1)

    n_assets = len(asset_names)
    n_periods = prices.shape[0] - 1

    # Assume monthly data (typical for Fama-French)
    periods_per_year = 12

    if rf is None:
        print(
            'Error: Input file does not contain a column for the risk-free rate (labeled "RF").'
        )
        sys.exit(1)

    # Find the efficient frontier of maximum return for a fixed ulcer index

    '''
    TODO: ulcer_mvo.py and Main.hs now have the same CAGR but they give slightly different optimization results
    '''
    print("=" * 70)
    print(f"EFFICIENT FRONTIER")
    print("=" * 70)
    print()
    frontier_uis, frontier_returns, frontier_weights = compute_efficient_frontier(
        target_ui=12,
        prices=prices,
        risk_free_rate=rf,
        periods_per_year=periods_per_year,
        allow_short=True,
        max_leverage=3,
    )
    if frontier_weights is None:
        print("error: efficient frontier optimization failed")
    else:
        result = OptimizationResult(
            weights=frontier_weights,
            portfolio_prices=compute_portfolio_prices(prices, frontier_weights, rf),
            periods_per_year=periods_per_year,
            utility=0,
            success=True,
            message="",
        )
        print_result(result)
    sys.exit(0)

    # Optimize using SLSQP (fastest reliable method)
    print("=" * 70)
    print(f"OPTIMIZATION RESULTS")
    print("=" * 70)
    print()

    print("Method: SLSQP (long-only)")
    print("-" * 50)

    result = optimize_portfolio(
        prices,
        risk_free_rate=rf,
        periods_per_year=periods_per_year,
        allow_short=False,
    )
    print_result(result)

    # Optimization with short selling allowed
    max_leverage = 2
    print(f"Method: SLSQP (with short selling, max leverage = {max_leverage})")
    print("-" * 50)

    result_short = optimize_portfolio(
        prices,
        risk_free_rate=rf,
        periods_per_year=periods_per_year,
        allow_short=True,
        max_leverage=max_leverage,
    )
    print_result(result_short)
