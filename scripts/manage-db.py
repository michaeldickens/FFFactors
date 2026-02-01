"""

manage-db.py
------------

Author: Michael Dickens <mdickens93@gmail.com>
Created: 2016-07-10

Takes just the desired columns from the big database and puts them into a smaller database.

Takes about 1.5 minutes to run on the Compustat db. CRSP db that only goes to 2007 has 15.5 million lines and takes about 5 minutes.

TODO: a lot of this stuff would probably work better as an awk script.

"""

import csv
from datetime import datetime
import math
import sys

import numpy as np
import sqlite3

compustat_keep = set([ 'gvkey', 'datadate', 'tic', 'datafqtr', 'gsector', 'ggroup', 'gind', 'gsubind', 'adjex', 'act', 'am', 'at', 'capx', 'ceq', 'che', 'cogs', 'csho', 'dlc', 'dltt', 'dlret', 'dp', 'dvp', 'dvpsx', 'icapt', 'lct', 'lt', 'mkvalt', 'mret', 'ni', 'oancf', 'oiadp', 'ppent', 'prcc', 'prstkc', 'pstkrv', 'rect', 'revt', 'sret', 'xsga', 'yret', 'indfmt', 'consol', 'popsrc', 'datafmt', 'costat'])

crsp_name_translator = {
    'permno': 'gvkey',
    'date': 'datadate',
    'ticker': 'tic',
    'prc': 'prcc',
    'cfacpr': 'adjex',
    'shrout': 'csho',
    'divamt': 'dvpsx',
}

def fix_colname(name_translator, c):
    global compustat_keep
    fixed = c.lower().replace('_c', '')
    if '_p' in fixed:
        return None  # ignore any cols that end in _p because they're duplicates of _c cols
    if fixed[-1] == 'q' and fixed[:-1] in compustat_keep:
        fixed = fixed[:-1]
    fixed = name_translator.get(fixed, fixed)  # if not found, do not change
    return fixed

def crsp_custom(vals):
    if vals['csho']:
        vals['csho'] = float(vals['csho']) / 1000
    if 'mkvalt' not in vals and vals['prcc'] and vals['csho']:
        vals['mkvalt'] = int(float(vals['prcc']) * float(vals['csho']))
    if vals['csho']:
        vals['csho'] = int(vals['csho'])


def slim_down_db(source, dest, keep=compustat_keep, keyname='gvkey', min_year=None, name_translator={}, drop_micro_caps=False):
    """
    Remove unwanted columns from db, including only those in `keep`.
    """
    print("Slimming down db...")
    with open(source, 'r') as pure_reader, open(dest, 'w') as pure_writer:
        reader = csv.reader(pure_reader)
        writer = csv.writer(pure_writer)
        header = None
        keep_cols = None
        insert_dummy_tic = False
        for line_num, row in enumerate(reader):
            if line_num % 1000 == 0:
                sys.stdout.write("\rOn line {}".format(line_num))

            if dest == "bigdata/CRSP-daily.csv" and min_year and keep_cols:
                date = datetime.strptime(row[1], "%Y/%m/%d")
                if date.year < min_year:
                    continue

            skip_this_row = False
            if header is None:
                # use header line to determine which columns to keep
                row = [fix_colname(name_translator, c) for c in row]
                if 'tic' not in row:
                    insert_dummy_tic = True
                    row.append('tic')
                keep_cols = [x is not None and x in keep for x in row]
                header = row
            else:
                if insert_dummy_tic:
                    row.append(str(row[header.index(keyname)]))
                vals = dict(zip(header, row))
                if name_translator:
                    crsp_custom(vals)

                date = datetime.strptime(vals['datadate'], "%Y/%m/%d")
                size_cutoff = 50 * 1.1**(date.year - 2015)
                if drop_micro_caps and ('mkvalt' in vals and float(vals['mkvalt']) < size_cutoff):
                    continue
            filtered_row = [row[i] for i in range(len(row)) if keep_cols[i]]
            writer.writerow(filtered_row)

        print("")


def quarterly_to_annual(source, dest):
    print("Converting quarterly to annual...")
    default_quarter = 4

    with open(source, 'rb') as pure_reader, open(dest, 'wb') as pure_writer:
        reader = csv.reader(pure_reader)
        writer = csv.writer(pure_writer)
        fqtr_index = None
        for line_num, row in enumerate(reader):
            if line_num % 1000 == 0:
                sys.stdout.write("\rOn line {}".format(line_num))

            skip_this_row = False
            if not fqtr_index:
                header = map(fix_colname, row)
                fqtr_index = header.index('fqtr')
            elif int(row[fqtr_index]) != default_quarter:
                skip_this_row = True

            if not skip_this_row:
                writer.writerow(row)

        print("")


def csv_to_sql(readfile, create_table=True):
    if "2007-2015" in readfile:
        date_format = "%Y/%m/%d"
    else:
        date_format = "%Y%m%d"

    with open(readfile) as reader:
        conn = sqlite3.connect("bigdata/CRSP-daily.db")
        c = conn.cursor()
        if create_table:
            c.execute("DROP TABLE IF EXISTS WeeklyPrices")
            c.execute('''
            CREATE TABLE WeeklyPrices (
            permno INT,
            date DATE,
            ticker TEXT,
            prc REAL,
            cfacpr REAL,
            PRIMARY KEY (permno, date)
            )
            ''')
            c.execute("CREATE INDEX TickerIndex ON WeeklyPrices (ticker)")

        prev_line = None
        for line_num, line in enumerate(reader):
            if line_num % 1000 == 0:
                sys.stdout.write("\rOn line {}".format(line_num))
            if line_num == 0:
                continue  # skip header line
            row = line.split(",")
            if not row[3].strip() or not row[4].strip() or prev_line == line:
                continue
            row[0] = int(row[0])
            row[1] = datetime.strptime(row[1], date_format)
            row[3] = abs(float(row[3]))  # abs b/c some prices are erroneously negative
            row[4] = float(row[4])
            if row[1].weekday() != 1:
                # only log rows on Tuesday (not Monday b/c Monday is often a trading holiday)
                continue
            c.execute('''
            INSERT INTO WeeklyPrices
            SELECT ?, ?, ?, ?, ?
            WHERE NOT EXISTS (SELECT 1 FROM WeeklyPrices WHERE permno = ? AND date = ?)
            ''', row + row[:2])
            prev_line = line

        conn.commit()
        conn.close()


def sql_make_tickers_table():
    conn = sqlite3.connect("bigdata/CRSP-daily.db")
    c = conn.cursor()
    c.execute("DROP TABLE IF EXISTS Tickers")
    c.execute('''
    CREATE TABLE Tickers (
      ticker TEXT PRIMARY KEY
    )
    ''')
    c.execute('''
    INSERT INTO Tickers
    SELECT DISTINCT ticker
    FROM WeeklyPrices
    ''')
    conn.commit()
    conn.close()


def load_all_to_sql():
    is_first = True
    # for name in ["1950-1970", "1970-1990", "1990-1999", "2000-2006", "2007-2015"]:
    for name in ["1950-1970"]:
        print("File:", name)
        csv_to_sql("bigdata/CRSP-daily/{}-slim.csv".format(name), create_table=is_first)
        is_first = False
    sql_make_tickers_table()


def slim_down_daily():
    for name in ["1950-1970", "1970-1990", "1990-1999", "2000-2006", "2007-2015"]:
        print("File:", name)
        # slim_down_db("/Volumes/Storage/WRDS/CRSP-daily/{}.csv".format(name), "bigdata/CRSP-daily/{}.csv".format(name), keep=crsp_keep)
        slim_down_db("bigdata/CRSP-daily/{}.csv".format(name), "bigdata/CRSP-daily/{}-slim.csv".format(name), keep=crsp_keep, keyname='permno', min_year=1950)


def add_lagged_prices_to_compustat_v1():
    """
    Use CRSP-monthly to add 6-month lagged prices to Compustat.
    """
    # TODO: If you look at Compustat-US.csv, the datadates aren't consistent.
    # Usually, datadate is %Y/12/31 with fyear %Y, but sometimes it's something
    # else, e.g. 1972/05/31 with fyear 1971. Is it possible that the
    # fundamentals data is already lagged?
    #
    # Typical value buys portfolio in June of year Y using fundamentals from
    # the fiscal year ending in calendar year Y-1. See "Common risk factors in
    # the returns on stocks and bonds", page 8
    # https://sci-hub.st/10.1016/0304-405x(93)90023-5

    # This is gonna be very memory-intensive, it might not even work. Might need to rewrite in Java or something.
    with open('resources/Compustat-US.csv', 'r') as infile:
        csvreader = csv.reader(infile)
        annual_data = [row for row in csvreader]
        annual_header = annual_data[0]
        annual_data = annual_data[1:]

    lagged_prices = {}

    with open('resources/Compustat-quarterly.csv', 'r') as infile:
        csvreader = csv.reader(infile)
        header = None
        indexes = {}
        for row in csvreader:
            if header is None:
                header = row
                indexes = {
                    k: crsp_header.index(k)
                    for k in ['gvkey', 'datadate', 'fyear', 'fqtr', 'dvpsx', 'mkvalt', 'prcc', 'adjex']
                }
            else:
                datadate = datetime.strptime(row[indexes['datadate']], '%Y/%m/%d')
                if datadate.month in [6, 7, 8]:
                    # Take the lagged price from the earliest reported price
                    # after June 1
                    key = (row[indexes['gvkey']], datadate.year)
                    real_time_data[key] = [
                        row[indexes[s]] for s in ['dvpsx', 'mkvalt', 'prcc', 'adjex']
                    ]

    # insert 6 month lagged prices into Compustat
    datadate_index = annual_header.index('datadate')
    for row in annual_data:
        datadate = datetime.strptime(row[datadate_index], '%Y/%m/%d')


def compustat_quarterly_to_annual(infile, outfile):
    """Convert Compustat quarterly db to an annual DB with 6 month lagged
    fundamentals.
    """
    with open(infile, 'r') as fp_reader, open(outfile, 'w') as fp_writer:
        csvreader = csv.reader(fp_reader)
        csvwriter = csv.writer(fp_writer)
        header = None
        output_row = None
        remaining_lag = None
        for line_num, row_list in enumerate(csvreader):
            if line_num % 1000 == 0:
                sys.stdout.write("\rOn line {}".format(line_num))

            if header is None:
                header = row_list
                csvwriter.writerow(header)
            elif remaining_lag == 0:
                row = dict(zip(header, row_list))
                if output_row['gvkey'] == row['gvkey']:
                    for key in ['csho', 'mkvalt', 'prcc', 'adjex']:
                        output_row[key] = row[key]

                    # Add up dividends across all 4 quarters
                    output_row['dvpsx'] += float(row['dvpsx'] or 0)
                    output_row['dvpsx'] = "{:.4f}".format(output_row['dvpsx'])

                    csvwriter.writerow(output_row.values())
                output_row = None
                remaining_lag = None
            else:
                row = dict(zip(header, row_list))
                datadate = datetime.strptime(row['datadate'], '%Y/%m/%d')
                if datadate.month in [10, 11, 12]:
                    output_row = row
                    output_row['datadate'] = "{}/06/30".format(datadate.year)
                    output_row['dvpsx'] = float(output_row['dvpsx'] or 0)
                    remaining_lag = 1
                elif remaining_lag is not None:
                    output_row['dvpsx'] += float(row['dvpsx'] or 0)
                    remaining_lag -= 1

        print("")


def compustat_quarterly_to_annual_v2(quarterly_infile, annual_infile, outfile):
    """
    For each Compustat-quarterly entry on year/06/30, find the Compustat-annual
    entry where FYEAR = year-1. Use the fundamentals from the Compustat-annual
    entry (including dividends) and the price data from Compustat-quarterly.

    This seems to fit with how French does it:
    https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_port_form_ep.html
    """
    with open(annual_infile, 'r') as fp_reader:
        annual_data = {}  # map from (gvkey, fyear) to row
        csvreader = csv.reader(fp_reader)
        annual_header = None
        for line_num, row_list in enumerate(csvreader):
            if line_num % 1000 == 0:
                sys.stdout.write("\rReading annual, line {}".format(line_num))
            if annual_header is None:
                annual_header = row_list
            else:
                row = dict(zip(annual_header, row_list))
                annual_data[(row['gvkey'], row['fyear'])] = row

    print("")

    missing_count = 0
    missing_gvkeys = set()

    with open(quarterly_infile, 'r') as fp_reader, open(outfile, 'w') as fp_writer:
        csvreader = csv.reader(fp_reader)
        csvwriter = csv.writer(fp_writer)
        header = None

        quarterly_cols_to_use = ['csho', 'mkvalt', 'prcc', 'adjex']

        for line_num, row_list in enumerate(csvreader):
            if line_num % 1000 == 0:
                sys.stdout.write("\rOn line {}".format(line_num))

            if header is None:
                header = row_list
                csvwriter.writerow(annual_header + quarterly_cols_to_use)
            else:
                row = dict(zip(header, row_list))
                datadate = datetime.strptime(row['datadate'], '%Y/%m/%d')
                if datadate.month in [6, 7, 8]:
                    annual_entry = annual_data.get((row['gvkey'], str(datadate.year - 1)))
                    if annual_entry is None:
                        missing_count += 1
                        missing_gvkeys.add(row['gvkey'])
                        continue

                    output_row = dict(**annual_entry)

                    # The date isn't actually 12/31, but it doesn't matter what
                    # month/day we use as long as it's the correct year, and
                    # Quote.hs treats annual series as always being on 12/31
                    output_row['datadate'] = "{}/12/31".format(datadate.year)
                    # output_row['datadate'] = "{}/06/30".format(datadate.year)
                    for key in quarterly_cols_to_use:
                        output_row[key] = row[key]

                    csvwriter.writerow(output_row.values())

    print("")
    print(missing_count, "entries not present in Compustat-annual, for", len(missing_gvkeys), "gvkeys")


# slim_down_db("bigdata/Compustat_annual.csv", "bigdata/Compustat-US.csv")
# slim_down_db("bigdata/CRSP_daily_full.csv", "bigdata/CRSP-daily.csv", keep=crsp_keep, min_year=1950)
# slim_down_db("bigdata/Compustat_global_quarterly.csv", "bigdata/Compustat_global_quarterly_slim.csv")
# quarterly_to_annual("bigdata/Compustat_global_quarterly_slim.csv", "bigdata/Compustat-global.csv")
# slim_down_db("bigdata/CRSP-monthly-full.csv", "bigdata/CRSP-monthly.csv", keep=crsp_name_translator.values(), keyname='permno', min_year=1950, name_translator=crsp_name_translator, drop_micro_caps=True)

# TODO redo slim_down_db to make sure compustat-quarterly has all the right fields

# slim_down_db('bigdata/Compustat_quarterly.csv', 'resources/Compustat-quarterly.csv')
compustat_quarterly_to_annual_v2('resources/Compustat-quarterly.csv', 'resources/Compustat-US.csv', 'resources/Compustat-lagged-2.csv')
