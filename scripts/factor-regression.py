"""factor-regression.py
---------

Author: Michael Dickens <mdickens93@gmail.com>
Created: 2021-01-30

DEPRECATED IN FAVOR OF lib/StockStrategy.Return.hs:multipleRegressionTTest or
factorRregression

"""

import csv

import numpy as np
from scipy.stats import t as tdist
from sklearn import linear_model
import statsmodels.api as sm


def prices_to_returns(prices):
    return [(prices[i] / prices[i - 1]) - 1 for i in range(1, len(prices))]


def clip_end(clip_len, xs):
    """Clip a list to include just the first `clip_len` entries."""
    return xs[:clip_len]


def clip_start(clip_len, xs):
    """Clip a list to include just the last `clip_len` entries."""
    return xs[(len(xs) - clip_len):]


def load_col(infile, col_name):
    res = []

    with open(infile, "r") as fp:
        reader = csv.reader(fp)
        header = None
        col_index = None
        for line in reader:
            if header is None:
                header = line
                col_index = header.index(col_name)
            else:
                res.append(float(line[col_index]))

    return res


def load_cols_avg(infile, col_names):
    """Load the equal-weighted average return of several columns."""
    cols = np.transpose([load_col(infile, x) for x in col_names])
    return [np.mean(x) for x in cols]


def load_factors():
    factors = []
    with open("resources/French/3_Factors.csv", "r") as fp:
        reader = csv.reader(fp)
        header = None
        for line in reader:
            if header is None:
                header = line
            else:
                factors.append(map(float, line))

    return factors


def difference(xs, ys):
    return [x - y for (x, y) in zip (xs, ys)]


hml_ew = difference(
    load_col("resources/French/Portfolios_BM_Equal_Wt.csv", "Hi 30"),
    load_col("resources/French/Portfolios_BM_Equal_Wt.csv", "Lo 30")
)

mom_ew = difference(
    load_cols_avg("resources/French/Portfolios_Momentum_Equal_Wt.csv", ["PRIOR 8", "PRIOR 9", "Hi PRIOR"]),
    load_cols_avg("resources/French/Portfolios_Momentum_Equal_Wt.csv", ["Lo PRIOR", "PRIOR 2", "PRIOR 3"])
)

if False:
    dependent_var_len = 75
    factors = [
        ("Beta", clip_start(dependent_var_len, load_col("resources/French/3_Factors.csv", "Mkt-RF"))),
        ("SMB", clip_start(dependent_var_len, load_col("resources/French/3_Factors.csv", "SMB"))),
        ("HML", clip_start(dependent_var_len, load_col("resources/French/3_Factors.csv", "HML"))),
    ]
    rf = clip_start(dependent_var_len, load_col("resources/French/3_Factors.csv", "RF"))


if True:
    factors = [
        ("QMHIX", clip_start(1832, prices_to_returns(load_col("resources/Live_Funds/QMHIX.csv", "adj_close")))),
        ("QSPIX", clip_start(1832, prices_to_returns(load_col("resources/Live_Funds/QSPIX.csv", "adj_close")))),
    ]


def intercept_pval(x, y):
    x2 = sm.add_constant(x)
    est = sm.OLS(y, x2)
    fit = est.fit()
    tstat = fit.tvalues[0]
    temp = tdist.cdf(tstat, len(y) - 1)
    return 2 * min(1 - temp, temp)


def factor_loading(name, infile, col_name, include_RF=True):
    factor_matrix = np.transpose([x[1] for x in factors])

    if type(col_name) == list:
        col = load_cols_avg(infile, col_name)
    else:
        col = load_col(infile, col_name)

    if col_name == "adj_close":
        # TODO: this is a hacky way of detecting when the input is prices not returns
        col = prices_to_returns(col)

    col = col[:-1]
    if include_RF:
        # Assume all cols start 1926-07, but potentially have different end dates.
        # Subtract RF b/c we only care about excess return.
        y = np.array(col[:len(factor_matrix)]) - np.array(rf)
    else:
        y = np.array(col[:len(factor_matrix)])


    reg = linear_model.LinearRegression()
    fit = reg.fit(factor_matrix, y)
    print(f"| {'':<15}|", end="")
    for factor_name, _ in factors:
        print(f" {factor_name} |", end="")
    print(" intercept | pval |")
    print(f"| {name:<15} |", end="")
    for c in fit.coef_:
        print(f" {c:.2f} |", end="")
    print(f" {fit.intercept_:.4f} | {intercept_pval(factor_matrix, y):.2g} |")

# factor_loading("ARKK", "resources/Live_Funds/ARKK-rets.csv", "ARKK")
factor_loading("QRPIX", "resources/Live_Funds/QRPIX.csv", "adj_close", include_RF=False)
