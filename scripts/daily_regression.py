"""

factor_regression.py
--------------------

Author  : Michael Dickens
Created : 2026-02-27

"""

"""
Factor regression of a dependent fund against one or more independent funds.
Reads CSV files with a date-format column header and an adj_close column,
computes daily returns, aligns on common dates, and runs OLS regression
to produce factor loadings (slopes) and annualized alpha.
"""

import pandas as pd
import numpy as np
import statsmodels.api as sm
from pathlib import Path


def read_csv(filepath: str) -> pd.Series:
    """
    Read a CSV that has at least two columns:
      - A date column whose header IS the date format string (e.g. "%Y-%m-%d")
      - A column named "adj_close"

    Returns a Series of adj_close indexed by datetime, sorted by date.
    """
    df = pd.read_csv(filepath)

    # The date-format column header will contain '%' — find it
    date_col = None
    date_fmt = None
    for col in df.columns:
        if "%" in col:
            date_col = col
            date_fmt = col
            break

    if date_col is None:
        raise ValueError(f"No date-format column found in {filepath}")
    if "adj_close" not in df.columns:
        raise ValueError(f"No 'adj_close' column found in {filepath}")

    df["date"] = pd.to_datetime(df[date_col], format=date_fmt)
    df = df.set_index("date").sort_index()

    return df["adj_close"].astype(float)


def compute_returns(prices: pd.Series) -> pd.Series:
    """Compute simple daily returns from a price series, dropping the first NaN."""
    return prices.pct_change().dropna()


def factor_regression(
    dependent_file: str,
    independent_files: list[str],
    dep_name: str | None = None,
    fund_names: list[str] | None = None,
    period_days: int = 1,
    trading_days_per_year: int = 252,
):
    """
    Run an OLS factor regression of a dependent fund's returns on
    one or more independent funds' returns.

    Parameters
    ----------
    dependent_file : path to the dependent fund CSV
    independent_files : list of paths to independent fund CSVs
    fund_names : optional friendly names for the independent funds
    period_days : sample prices every N trading days before computing returns
                  (e.g. 5 for weekly). Reduces noise from EOD pricing differences.
    trading_days_per_year : used to annualize alpha (default 252)

    Returns
    -------
    dict with keys: 'model' (statsmodels result), 'alpha_daily', 'alpha_annualized',
    'slopes' (dict mapping fund name -> beta), 'summary' (text)
    """
    if fund_names is None:
        fund_names = [Path(f).stem for f in independent_files]
    if dep_name is None:
        dep_name = Path(dependent_file).stem

    # Read prices and align on common dates first
    dep_prices = read_csv(dependent_file)
    indep_prices = {
        name: read_csv(fpath) for name, fpath in zip(fund_names, independent_files)
    }

    all_prices = pd.DataFrame({"dependent": dep_prices, **indep_prices}).dropna()

    # Subsample every N-th trading day, then compute returns
    if period_days > 1:
        all_prices = all_prices.iloc[::period_days]

    all_returns = all_prices.pct_change().dropna()

    periods_per_year = trading_days_per_year / period_days
    period_label = f"every {period_days} trading days" if period_days > 1 else "daily"
    print(
        f"Observations: {len(all_returns)} ({period_label}, "
        f"{all_prices.index.min().date()} to {all_prices.index.max().date()})"
    )

    y = all_returns["dependent"]
    X = all_returns[fund_names]
    X = sm.add_constant(X)  # adds 'const' column = daily alpha

    model = sm.OLS(y, X).fit()

    alpha_daily = model.params["const"]
    # Annualize: (1 + per-period alpha)^(periods_per_year) - 1
    alpha_annual = (1 + alpha_daily) ** periods_per_year - 1

    slopes = {name: model.params[name] for name in fund_names}
    dep_name = dep_name or Path(dependent_file).stem

    return {
        "dep_name": dep_name,
        "fund_names": fund_names,
        "model": model,
        "alpha_daily": alpha_daily,
        "alpha_annualized": alpha_annual,
        "slopes": slopes,
        "n_obs": len(all_returns),
        "date_range": (all_returns.index.min().date(), all_returns.index.max().date()),
        "summary": model.summary().as_text(),
    }


def format_regression(
    result: dict, eq_width: int = 0, stats_width: int = 0
) -> tuple[str, str]:
    """
    Format a regression result as two aligned lines:
      Line 1 (equation):  RSSB = 1.02 * VT + 0.74 * GOVT
      Line 2 (stats):         (R² = 0.8991, α = -5.43%, t = -1.28, p = 0.2014)

    Parameters
    ----------
    result : dict returned by factor_regression()
    eq_width : minimum width to pad the equation line to (for alignment)
    stats_width : minimum width to pad the stats line to (for alignment)

    Returns
    -------
    (equation_str, stats_str) — the two lines, padded to at least the given widths
    """
    model = result["model"]
    dep = result["dep_name"]
    names = result["fund_names"]
    slopes = result["slopes"]

    # Build equation: DEP = coeff * NAME + coeff * NAME + ...
    terms = []
    for name in names:
        coeff = slopes[name]
        terms.append(f"{coeff:5.2f} * {name}")
    equation = f"{dep} = {' + '.join(terms)}"

    # Build stats line, indented to sit under the equation
    r2 = model.rsquared
    alpha_ann = result["alpha_annualized"]
    t_alpha = model.tvalues["const"]
    p_alpha = model.pvalues["const"]
    stats = f"(R² = {r2:.4f}, α = {alpha_ann:+.2%}, t = {t_alpha:.2f}, p = " + (
        f"{p_alpha:.3f})" if p_alpha > 0.001 else f"{p_alpha:.1e}"
    )

    # Pad both lines to requested widths
    equation = equation.ljust(eq_width)
    stats = stats.ljust(stats_width)

    return equation, stats


def print_regressions(results: list[dict]):
    """
    Pretty-print one or more regression results with columns aligned.

    Pass a list of dicts returned by factor_regression().
    """
    if not results:
        return

    # First pass: compute max widths
    raw = [format_regression(r) for r in results]
    max_eq = max(len(eq) for eq, _ in raw)
    max_st = max(len(st) for _, st in raw)

    # Second pass: format with alignment and print
    for r in results:
        eq, st = format_regression(r, eq_width=max_eq, stats_width=max_st)
        indent = " " * (eq.index("=") + 2)  # align stats under RHS of equation
        print(eq)
        print(f"{indent}{st}")
        print()


if __name__ == "__main__":
    results = []

    dependent_fund = "Apex"
    independent_funds = "QMNIX QGMIX ADAIX".split()
    dependent_file = f"resources/AQR/{dependent_fund}.csv"
    independent_files = [
        f"resources/Live_Funds/{name}.csv" for name in independent_funds
    ]

    results.append(
        factor_regression(
            dependent_file,
            independent_files,
            "Apex (daily) ",
            independent_funds,
            period_days=1,
        )
    )

    results.append(
        factor_regression(
            dependent_file,
            independent_files,
            "Apex (weekly)",
            independent_funds,
            period_days=5,
        )
    )

    print("\n" + "=" * 60)
    print("COMPACT SUMMARY")
    print("=" * 60 + "\n")
    print_regressions(results)
