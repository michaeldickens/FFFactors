"""
factor_regression.py
--------------------

Author  : Michael Dickens
Created : 2026-02-27

Factor regression of a dependent fund against one or more independent funds.
Based on src/Regression.hs, but supports daily or weekly regressions, not just
monthly.

Written with help from Claude Opus 4.6.

"""

import pandas as pd
import numpy as np
import statsmodels.api as sm
from pathlib import Path
from pykalman import KalmanFilter


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
    fund_names: list[str] | None = None,
    dep_name: str | None = None,
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
    indep_prices = {name: read_csv(fpath)
                    for name, fpath in zip(fund_names, independent_files)}

    all_prices = pd.DataFrame({"dependent": dep_prices, **indep_prices}).dropna()

    # Subsample every N-th trading day, then compute returns
    if period_days > 1:
        all_prices = all_prices.iloc[::period_days]

    all_returns = all_prices.pct_change().dropna()

    periods_per_year = trading_days_per_year / period_days
    period_label = (f"every {period_days} trading days"
                    if period_days > 1 else "daily")
    print(f"Observations: {len(all_returns)} ({period_label}, "
          f"{all_prices.index.min().date()} to {all_prices.index.max().date()})")

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


def format_regression(result: dict, eq_width: int = 0, stats_width: int = 0) -> tuple[str, str]:
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
        terms.append(f"{coeff:.2f} * {name}")
    equation = f"{dep} = {' + '.join(terms)}"

    # Build stats line, indented to sit under the equation
    r2 = model.rsquared
    alpha_ann = result["alpha_annualized"]
    t_alpha = model.tvalues["const"]
    p_alpha = model.pvalues["const"]
    stats = f"(R² = {r2:.4f}, α = {alpha_ann:+.2%}, t = {t_alpha:.2f}, p = {p_alpha:.4f})"

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


# ── Kalman filter: time-varying regression ───────────────────────────────────

from statsmodels.tsa.statespace.mlemodel import MLEModel


class TimeVaryingRegression(MLEModel):
    """State-space model for linear regression with time-varying coefficients.

    State vector: [alpha_t, beta1_t, beta2_t, ...]
    Observation:  y_t = [1, x1_t, x2_t, ...] @ state_t + obs_noise

    The states follow random walks:
        state_t = state_{t-1} + process_noise

    MLE estimates the observation noise variance and the process noise
    variances (one per state), controlling how fast each coefficient drifts.

    TODO(MD): I think this code is wrong because pykalman gives different
    results.
    """

    def __init__(self, y, X, state_names=None, **kwargs):
        """
        Parameters
        ----------
        y : array-like, shape (n_obs,)
            Dependent variable (returns).
        X : array-like, shape (n_obs, k)
            Independent variables WITH a leading constant column.
        state_names : list of str, optional
            Names for the states (e.g. ["alpha", "VT", "GOVT"]).
        """
        self._X = np.asarray(X)
        n, k = self._X.shape

        # Build the time-varying design matrix: shape (1, k, n)
        design = np.zeros((1, k, n))
        for t in range(n):
            design[0, :, t] = self._X[t, :]

        super().__init__(
            endog=y,
            k_states=k,
            k_posdef=k,
            design=design,
            **kwargs,
        )

        if state_names is not None:
            self._state_names = state_names

        # Transition: identity (random walk)
        self["transition"] = np.eye(k)
        # Selection: identity (each state has its own noise)
        self["selection"] = np.eye(k)

        # Initial state: approximate diffuse (large initial variance)
        self.initialize_approximate_diffuse()

    @property
    def param_names(self):
        k = self.k_states
        return ["sigma2_obs"] + [f"sigma2_state_{i}" for i in range(k)]

    @property
    def start_params(self):
        # Reasonable starting values: obs noise ~ variance of y, state noise small
        k = self.k_states
        var_y = np.var(self.endog)
        return np.array([var_y] + [var_y * 0.01] * k)

    def update(self, params, **kwargs):
        params = super().update(params, **kwargs)

        # Observation noise variance
        self["obs_cov", 0, 0] = params[0]

        # Process noise: diagonal covariance
        self["state_cov"] = np.diag(params[1:])

    def transform_params(self, unconstrained):
        """Ensure all variance params are positive."""
        return unconstrained ** 2

    def untransform_params(self, constrained):
        return constrained ** 0.5



def _prepare_tv_data(dependent_file, independent_files, fund_names, dep_name, period_days):
    """Shared data prep for time-varying regression: read, align, subsample, return returns."""
    if fund_names is None:
        fund_names = [Path(f).stem for f in independent_files]
    if dep_name is None:
        dep_name = Path(dependent_file).stem

    dep_prices = read_csv(dependent_file)
    indep_prices = {name: read_csv(fpath)
                    for name, fpath in zip(fund_names, independent_files)}
    all_prices = pd.DataFrame({"dependent": dep_prices, **indep_prices}).dropna()
    if period_days > 1:
        all_prices = all_prices.iloc[::period_days]
    all_returns = all_prices.pct_change().dropna()

    y = all_returns["dependent"].values
    X = sm.add_constant(all_returns[fund_names].values)
    dates = all_returns.index
    state_names = ["alpha"] + fund_names

    return y, X, dates, state_names, dep_name, fund_names


def _tv_summary_and_plot(states, states_lower, states_upper, state_names, dates,
                         dep_name, plot, plot_suffix=""):
    """Shared printing and plotting for time-varying regression results."""
    print(f"\nTime-varying betas for {dep_name}:")
    print(f"{'':>10s}  {'start':>10s}  {'end':>10s}  {'mean':>10s}  {'std':>10s}")
    for col in state_names:
        s = states[col]
        print(f"{col:>10s}  {s.iloc[0]:+10.4f}  {s.iloc[-1]:+10.4f}  "
              f"{s.mean():+10.4f}  {s.std():10.4f}")

    if plot:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt

        n_states = len(state_names)
        fig, axes = plt.subplots(n_states, 1, figsize=(12, 3 * n_states),
                                 sharex=True)
        if n_states == 1:
            axes = [axes]

        for ax, col in zip(axes, state_names):
            ax.plot(dates, states[col], label=col, linewidth=1.5)
            ax.fill_between(dates, states_lower[col], states_upper[col],
                            alpha=0.2)
            ax.set_ylabel(col)
            ax.legend(loc="upper right")
            ax.grid(True, alpha=0.3)

        axes[0].set_title(f"Time-varying factor loadings: {dep_name}")
        axes[-1].set_xlabel("Date")
        plt.tight_layout()

        safe_name = dep_name.replace(" ", "_")
        plot_path = f"images/{safe_name}_tv_betas{plot_suffix}.png"
        fig.savefig(plot_path, dpi=150)
        plt.close(fig)
        print(f"\nPlot saved to {plot_path}")


def tv_factor_regression(
    dependent_file: str,
    independent_files: list[str],
    fund_names: list[str] | None = None,
    dep_name: str | None = None,
    period_days: int = 5,
    plot: bool = True,
):
    """
    Time-varying factor regression using statsmodels Kalman filter (MLEModel).

    MLE estimates observation and process noise variances. Smoothed (two-pass)
    state estimates give the optimal beta at each time step given all data.

    Returns dict with 'states', 'states_lower', 'states_upper', 'model_result'.
    """
    y, X, dates, state_names, dep_name, fund_names = _prepare_tv_data(
        dependent_file, independent_files, fund_names, dep_name, period_days)

    print(f"Fitting Kalman filter (statsmodels MLE): {len(y)} observations, "
          f"{len(state_names)} states ({', '.join(state_names)})")

    model = TimeVaryingRegression(y, X, state_names=state_names)
    result = model.fit(disp=False)

    # Smoothed states: optimal estimate using ALL data (forward + backward pass)
    states = pd.DataFrame(result.smoothed_state.T, index=dates, columns=state_names)

    n_states = len(state_names)
    std_errors = np.zeros((len(dates), n_states))
    for t in range(len(dates)):
        std_errors[t, :] = np.sqrt(np.diag(result.smoothed_state_cov[:, :, t]))

    states_lower = pd.DataFrame(
        result.smoothed_state.T - 1.96 * std_errors, index=dates, columns=state_names)
    states_upper = pd.DataFrame(
        result.smoothed_state.T + 1.96 * std_errors, index=dates, columns=state_names)

    _tv_summary_and_plot(states, states_lower, states_upper, state_names, dates,
                         dep_name, plot, plot_suffix="_mle")

    return {
        "states": states,
        "states_lower": states_lower,
        "states_upper": states_upper,
        "model_result": result,
    }


def tv_factor_regression_pykalman(
    dependent_file: str,
    independent_files: list[str],
    fund_names: list[str] | None = None,
    dep_name: str | None = None,
    period_days: int = 5,
    n_em_iter: int = 20,
    plot: bool = True,
):
    """
    Time-varying factor regression using pykalman (EM algorithm).

    EM iteratively estimates the noise covariances. Smooth gives the
    optimal beta at each time step given all data.

    Parameters
    ----------
    n_em_iter : number of EM iterations for fitting noise parameters

    Returns dict with 'states', 'states_lower', 'states_upper', 'kf'.
    """
    y, X, dates, state_names, dep_name, fund_names = _prepare_tv_data(
        dependent_file, independent_files, fund_names, dep_name, period_days)

    n_obs, k = X.shape

    print(f"Fitting Kalman filter (pykalman EM, {n_em_iter} iterations): "
          f"{n_obs} observations, {k} states ({', '.join(state_names)})")

    kf = KalmanFilter(
        transition_matrices=np.eye(k),
        observation_matrices=X[:, np.newaxis, :],   # (n_obs, 1, k)
        em_vars=["transition_covariance", "observation_covariance",
                 "initial_state_mean", "initial_state_covariance"],
    )
    kf = kf.em(y, n_iter=n_em_iter)
    smoothed_states, smoothed_covs = kf.smooth(y)

    # smoothed_states: (n_obs, k), smoothed_covs: (n_obs, k, k)
    states = pd.DataFrame(smoothed_states, index=dates, columns=state_names)

    std_errors = np.sqrt(np.array([np.diag(smoothed_covs[t]) for t in range(n_obs)]))

    states_lower = pd.DataFrame(
        smoothed_states - 1.96 * std_errors, index=dates, columns=state_names)
    states_upper = pd.DataFrame(
        smoothed_states + 1.96 * std_errors, index=dates, columns=state_names)

    _tv_summary_and_plot(states, states_lower, states_upper, state_names, dates,
                         dep_name, plot, plot_suffix="_pykalman")

    return {
        "states": states,
        "states_lower": states_lower,
        "states_upper": states_upper,
        "kf": kf,
    }


if __name__ == "__main__":
    results = []

    dependent_fund = "Apex"
    independent_funds = "QMNIX QGMIX ADAIX".split()
    dependent_file = f"resources/Live_Funds/{dependent_fund}.csv"
    independent_files = [
        f"resources/Live_Funds/{name}.csv" for name in independent_funds
    ]

    files = dict(
        dependent_file=f"resources/Live_Funds/{dependent_fund}.csv",
        independent_files=independent_files,
        fund_names=independent_funds,
        dep_name=dependent_fund,
    )

    # Weekly (every 5 trading days)
    results.append(factor_regression(**files, period_days=5))

    print("\n" + "=" * 60)
    print("COMPACT SUMMARY")
    print("=" * 60 + "\n")
    print_regressions(results)

    # Time-varying regression
    print("\n" + "=" * 60)
    print("TIME-VARYING REGRESSION (Kalman filter)")
    print("=" * 60 + "\n")
    tv = tv_factor_regression_pykalman(**files, period_days=5)
