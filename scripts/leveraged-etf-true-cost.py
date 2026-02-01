"""

leveraged-etf-true-cost.py
--------------------------

Author: Michael Dickens <michael@mdickens.me>
Created: 2025-01-13

DEPRECATED. For latest version, see ~/programs/leveraged-etfs

Original calculations in ~/Documents/Finance/true cost of leveraged ETFs.xlsx

Note: Fees are not included in calculations because ETF returns already include fees.

"""

import csv
from datetime import datetime
from dateutil.relativedelta import relativedelta


def prices_to_returns(prices):
    return [(prices[i] / prices[i - 1]) - 1 for i in range(1, len(prices))]


def get_prices(ticker):
    with open(f"resources/Live_Funds/{ticker}.tsv", "r") as f:
        reader = csv.reader(f, delimiter="\t")
        prices_dict = {}
        header = next(reader)
        for row in reader:
            date = datetime.strptime(row[0].strip(), "%b %d, %Y")
            adj_close = float(row[5])
            prices_dict[date] = adj_close

    desired_dates = [datetime(2024, month, 1) for month in range(1, 13)] + [
        datetime(2025, 1, 1)
    ]
    prices = [prices_dict[date] for date in desired_dates]
    prices = [price / prices[0] for price in prices]
    return prices


rssb = get_prices("RSSB")
vt = get_prices("VT")
govt = get_prices("GOVT")
rf = prices_to_returns(get_prices("BIL"))

simulated_prices = [1]
simulated_props = [(1, 1)]
for i in range(1, len(vt)):
    vt_factor = vt[i] / vt[i - 1]
    govt_factor = govt[i] / govt[i - 1]
    new_props = (
        simulated_props[-1][0] * vt_factor,
        simulated_props[-1][1] * govt_factor,
    )
    net_earnings = (new_props[0] + new_props[1]) - (
        simulated_props[-1][0] + simulated_props[-1][1]
    )
    cost_of_leverage = ((new_props[0] + new_props[1]) - 1) * rf[i - 1]
    new_price = simulated_prices[-1] + net_earnings - cost_of_leverage
    simulated_prices.append(new_price)

    rebalanced_new_props = (
        1 * new_price
        if new_props[0] <= 0.95 * new_price or new_props[0] >= 1.05 * new_price
        else new_props[0],
        1 * new_price
        if new_props[1] <= 0.95 * new_price or new_props[1] >= 1.05 * new_price
        else new_props[1],
    )
    simulated_props.append(rebalanced_new_props)


for i in range(1, len(rssb)):
    rssb_pct = 100 * (rssb[i] / rssb[i - 1] - 1)
    simulated_pct = 100 * (simulated_prices[i] / simulated_prices[i - 1] - 1)
    vt_pct = 100 * (vt[i] / vt[i - 1] - 1)
    govt_pct = 100 * (govt[i] / govt[i - 1] - 1)
    print(
        f"{rssb_pct:5.2f}% --> {simulated_pct:5.2f}% ~ {vt_pct + govt_pct:5.2f}% = {vt_pct:5.2f}% + {govt_pct:5.2f}%    ({simulated_props[i][0]:.3f}, {simulated_props[i][1]:.3f})"
    )

rssb_pct = 100 * (rssb[-1] / rssb[0] - 1)
simulated_pct = 100 * (simulated_prices[-1] / simulated_prices[0] - 1)

# Add 0.14% fee for BIL back in. I'm just using BIL as the risk-free rate
# because it's in the same data format which makes coding easier
simulated_pct += 0.14

print(f"\nFinal Returns -- RSSB: {rssb_pct:5.2f}%, Simulated: {simulated_pct:5.2f}%")
