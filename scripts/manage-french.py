"""

manage-french.py
----------------

Author: Michael Dickens <michael@mdickens.me>
Created: 2023-02-02

See ManageFrench.hs for newer stuff.

"""

import csv
from collections import defaultdict
from datetime import datetime, timedelta
from decimal import Decimal
from functools import reduce
import numpy as np


def read_csv(infile):
    """Return a list of dicts"""

    data_rows = []
    with open(infile, 'r') as fp:
        reader = csv.reader(fp)
        header = None
        for row in reader:
            if not header:
                header = row
            else:
                data_rows.append(dict(zip(header, row)))

    return data_rows


def commodity_returns_to_percents():
    with open("resources/French/AQR_Commodities_Old.CSV") as fp:
        is_header = True
        for line in fp:
            vals = line.split(',')
            if not is_header:
                for i in range(1, 10):
                    vals[i] = str(100 * float(vals[i]))
            print(','.join(vals),)

            is_header = False


def treasury_yields_to_returns():
    '''
    Created 2020-01-12. Incomplete, see TODOs below

    See https://www.mdpi.com/2306-5729/4/3/91 for how to convert yield to return. ("Treasury Bond Return Data Starting in 1962")

    Results from that paper are in 10Y-Treasury-Returns.csv.
    '''
    months_to_maturity = {
        '1-month': 1,
        '3-month': 3,
        '2-year': 24,
        '10-year': 120,
        '30-year': 360,
        '3-month secondary': 3,
        '1-year secondary': 12,
    }


    yield_series = defaultdict(list)
    with open("resources/French/Treasury-Yields-Small.csv") as fp:
        header = None
        for line in fp:
            vals = line.split(',')
            if header is None:
                vals = header
            else:
                for i in range(len(vals)):
                    yield_series[header[i]].append(float(vals[i]) if vals[i] else None)

    # Convert yields to prices
    price_series = defaultdict(list)
    for k in yield_series:
        for i in range(len(yield_series)):
            # TODO: convert each yield to a price
            pass

    # TODO: convert prices+yields to returns


def rore_to_french():
    '''
    Convert RORE data to the French format by separating each country into its
    own file and then removing the country columns.

    Created 2020-05-03.
    '''
    country_output = defaultdict(list)
    with open("resources/RORE/rore_public_supplement.csv") as rore_file:
        reader = csv.reader(rore_file)
        header = None
        for row in reader:
            if header is None:
                header = ["%Y-%m-%d"] + row[4:]  # FrenchQuote.hs uses date header to determine date format
            else:
                country_code = row[1]
                trimmed_row = row[3:]

                # convert from absolute to percent, to match French format
                for i in range(1, len(trimmed_row)):
                    if trimmed_row[i] != '':
                        # without using formatted output, decimal will print with a trailing 00
                        trimmed_row[i] = "{:.5f}".format(Decimal(trimmed_row[i]) * 100)
                country_output[row[1]].append(trimmed_row)

    for country_code in country_output:
        with open("resources/RORE/countries/{}_supplement.csv".format(country_code), 'w') as fp:
            writer = csv.writer(fp)
            writer.writerow(header)
            for row in country_output[country_code]:
                writer.writerow(row)


def make_series_for_each_day(infile, outdir):
    data_rows = []

    with open(infile, 'r') as fp:
        reader = csv.reader(fp)
        header = None
        for row in reader:
            if not header:
                header = row
                header[0] = "%Y%m"
                continue

            date = datetime.strptime(row[0], "%Y%m%d")
            rets = [float(x) for x in row[1:]]
            data_rows.append([date] + rets)

    for start_day_offset in range(22):
        month_groups = []
        monthly_data = []

        for i in range(len(data_rows)//22):
            month_groups.append(data_rows[(start_day_offset + 22*i):(start_day_offset + 22*(i+1))])

        fake_date = 1000 * 12
        for month in month_groups:
            monthly_row = ["{:04}{:02}".format(fake_date // 12, 1 + fake_date % 12)]
            for col in range(1, len(month[0])):
                total_ret = np.prod([1 + row[col]/100 for row in month]) - 1
                monthly_row.append(total_ret)

            monthly_data.append(monthly_row)
            fake_date += 1

        with open("{}/day{}.csv".format(outdir, start_day_offset + 1), 'w') as fp:
            writer = csv.writer(fp)
            writer.writerow(header)

            for row in monthly_data:
                row = [row[0]] + ["{:.2f}".format(100 * x) for x in row[1:]]
                writer.writerow(row)


def daily_volatility(infile, outfile, lookback_days=60):
    def daily_rets_to_monthly(daily_rets, end_index):
        return 100 * (reduce(
            lambda x, y: x * y,
            [1 + x/100 for x in daily_rets[days_per_month*(end_index-1):days_per_month*end_index]]
        ) - 1)


    data_rows = []

    with open(infile, 'r') as fp:
        reader = csv.reader(fp)
        header = None
        for row in reader:
            if not header:
                header = row
                header[0] = "%Y%m"
                continue

            date = datetime.strptime(row[0].strip(), "%Y%m%d")
            rets = [float(x) for x in row[1:]]
            data_rows.append([date] + rets)

    with open(outfile, 'w') as fp:
        writer = csv.writer(fp)
        writer.writerow(["%Y%m", "RF", "Mkt", "{}DayVolatility".format(lookback_days)])

        days_per_month = 22
        fake_date = 1000 * 12
        for i in range(1, len(data_rows)//days_per_month):
            date_str = "{:04}{:02}".format(fake_date // 12, 1 + fake_date % 12)
            daily_rets = [row[1] + row[4] for row in data_rows]  # Mkt-RF + RF
            rf = daily_rets_to_monthly([row[4] for row in data_rows], i)
            ret = daily_rets_to_monthly(daily_rets, i)
            vol = np.std(daily_rets[days_per_month*(i-1):days_per_month*i])
            monthly_row = [date_str, "{:.2f}".format(rf), "{:.2f}".format(ret), "{:.2f}".format(vol)]
            writer.writerow(monthly_row)
            fake_date += 1


def crypto_daily_to_monthly(currency_name):
    infile = '../resources/crypto/Daily_Coinbase_{}USD.csv'.format(currency_name)
    outfile = '../resources/crypto/{}.csv'.format(currency_name)

    data_rows = read_csv(infile)

    symbol = data_rows[0]['symbol']  # e.g., "BTC/USD"
    data_rows.reverse()

    monthly_rows = []
    prev_price = None
    daily_prices = []

    for row in data_rows:
        date = datetime.strptime(row['date'], "%Y-%m-%d")

        # range includes (eg) 2020-01-01 and 2020-02-01
        daily_prices.append(row['close'])

        if date.day == 1:
            # Unlike stock markets, it looks like Coinbase is always open on
            # the 1st of the month
            if not prev_price:
                prev_price = float(row['close'])
                continue
            return_percent = 100 * (float(row['close']) / prev_price - 1)
            daily_returns = [float(x)/float(y) - 1 for x, y in zip(daily_prices[1:], daily_prices)]
            daily_stdev = np.std(daily_returns)
            monthly_rows.append([
                date.strftime("%Y-%m"),
                "{:.2f}".format(return_percent),
                "{:.2f}".format(100 * daily_stdev),
            ])
            prev_price = float(row['close'])
            daily_prices = [row['close']]

    with open(outfile, 'w') as fp:
        writer = csv.writer(fp)
        writer.writerow(["%Y-%m", symbol, symbol + "_Daily_Stdev"])

        for row in monthly_rows:
            writer.writerow(row)


def reformat_consolidated_coin_data():
    infile = '../resources/crypto/consolidated_coin_data_original.csv'
    outfile = '../resources/crypto/consolidated_coin_data.csv'

    # Prepare date dict
    coin_prices_by_date = {}
    for delta in range(5000):
        date = datetime(2008, 1, 1) + timedelta(days=delta)
        if date.day == 1:
            coin_prices_by_date[date] = {}

    all_currencies = set()
    with open(infile, 'r') as fp:
        reader = csv.reader(fp)
        header = None
        for row in reader:
            if not header:
                header = row
                continue

            data_dict = dict(zip(header, row))
            date = datetime.strptime(data_dict['Date'], "%b %d, %Y")
            if date.day != 1:
                continue
            coin_prices_by_date[date][data_dict['Currency']] = float(data_dict['Close'].replace(',', ''))
            if data_dict['Currency'] not in all_currencies:
                all_currencies.add(data_dict['Currency'])

    # Convert prices to returns
    prev_prices = {}
    coin_returns_by_date = {}
    for date in coin_prices_by_date:
        coin_prices = coin_prices_by_date[date]
        coin_returns = {}
        for currency in coin_prices:
            if currency not in prev_prices:
                continue
            coin_returns[currency] = coin_prices[currency] / prev_prices[currency] - 1
        prev_prices = coin_prices
        coin_returns_by_date[date] = coin_returns

    # output to new file
    with open(outfile, 'w') as fp:
        writer = csv.writer(fp)
        header = ["%Y-%m"] + list(all_currencies)
        writer.writerow(header)
        for date in coin_returns_by_date:
            coin_returns = coin_returns_by_date[date]
            if len(coin_returns) == 0:
                continue
            row = [date.strftime("%Y-%m")]
            for currency in list(all_currencies):
                if currency in coin_returns:
                    row.append("{:.2f}".format(100 * coin_returns[currency]))
                else:
                    row.append("")
            print("writing {}".format(date))
            writer.writerow(row)


if __name__ == '__main__':
    crypto_daily_to_monthly("BTC")
