"""

CAPS.py
-------

Author: Michael Dickens <mdickens93@gmail.com>
Created: 2017-03-18

"""

import csv
from datetime import date, timedelta

from yahoo_finance import Share, YQLResponseMalformedError

def get_closest_quote(share, date):
    """
    Finds the closest quote moving backward in time.
    """
    quotes = share.get_historical(str(date - timedelta(days=5)), str(date))
    return float(quotes[-1]['Adj_Close'])

def stuff():
    start_date = date(2013, 7, 22)

    with open('/Users/MTGAP/Documents/Finance/Stocks-CAPS.csv', 'r') as csvfile:
        reader = csv.reader(csvfile)
        header = None
        data = []
        for row in reader:
            if header is None:
                header = row
            else:
                data.append(dict(zip(header, row)))

    outperform_picks = []

    with open('CAPS-rets.csv', 'w') as writefile:
        for stock in data:
            share = Share(stock['Symbol'])
            try:
                start_price = get_closest_quote(share, start_date)
                end_price = get_closest_quote(share, start_date + timedelta(days=365))
                ret_1yr = (end_price - start_price) / start_price
                outperforms = int(stock['All-Star Outperform Picks'])
                outperform_picks.append((outperforms, ret_1yr))
                writefile.write('{},{},{}\n'.format(stock['Symbol'], ret_1yr, outperforms))
            except YQLResponseMalformedError:
                print "broke on {}".format(stock['Symbol'])

    outperform_picks.sort(key=lambda x: x[0])

    for i in range(10):
        length = len(outperform_picks) / 10
        decile = outperform_picks[(length*i):(length*(i+1))]
        avg_ret = sum([ret for (outperforms, ret) in decile])
        print "{}: {}".format(i, avg_ret)

def pare_down_csv():
    with open('/Users/MTGAP/Documents/Finance/Stocks-CAPS.csv', 'r') as csvfile:
        reader = csv.reader(csvfile)
        header = None
        data = []
        for row in reader:
            if header is None:
                header = row
            else:
                data.append(dict(zip(header, row)))

    with open('resources/CAPS.csv', 'w') as writefile:
        cols = "Symbol,Wall Street Underperforms,All-Star Picks,Underperform Picks,Wall Street Outperforms,All Star Underperform Picks,Outperform Picks,All-Star Outperform Picks".split(',')
        writefile.write(','.join(cols) + "\n")
        for row in data:
            writefile.write(','.join([str(row[c]) for c in cols]) + "\n")

pare_down_csv()
