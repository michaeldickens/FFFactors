import csv
from collections import defaultdict
from datetime import datetime, timedelta
from decimal import Decimal
from functools import reduce
import numpy as np


def read_cowles(infile):
    with open(infile, 'r') as fp:
        reader = csv.reader(fp)
        header = None
        rows = []
        for row in reader:
            if header is None:
                header = row
            else:
                rows.append(row)

        flat_rows = {}
        for row in rows:
            for i in range(1, len(row)):
                if int(row[0]) <= 1925 and header[i] != '1 All Stocks':
                    flat_rows[(row[0], header[i])] = row[i]

    return flat_rows


def flatten_cowles():
    earnings = read_cowles('../resources/Cowles/earnings.csv')
    ep = read_cowles('../resources/Cowles/ep.csv')
    price = read_cowles('../resources/Cowles/price.csv')
    div_yield = read_cowles('../resources/Cowles/yield.csv')

    out_rows = {}
    for k in earnings:
        out_rows[k] = [
            price[k],
            div_yield[k],
            earnings[k],
            div_yield[k],
        ]

    tickers = list(set(x[1] for x in out_rows.keys()))
    tickers.sort(key=lambda s: int(s.split()[0]))
    gvkeys = dict(zip(tickers, range(1, len(tickers) + 1)))

    with open('../resources/Cowles/Cowles.csv', 'w') as fp:
        writer = csv.writer(fp)
        # 'am' is actually E/P
        writer.writerow(['datadate', 'gvkey', 'tic', 'prcc', 'dvpsx', 'ni', 'am'])

        for k in out_rows:
            if price[k] and div_yield[k]:
                dividend = "{:.4f}".format(float(price[k]) * float(div_yield[k]) / 100)
            else:
                dividend = ''
            writer.writerow([
                "{}/12/31".format(k[0]),
                gvkeys[k[1]],
                k[1],  # ticker
                price[k],
                dividend,
                earnings[k],
                ep[k],
            ])


flatten_cowles()
