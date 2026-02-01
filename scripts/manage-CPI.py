"""
manage-CPI.py
-------------

Author: Michael Dickens
Created: 2025-02-26

CPI downloaded from here: https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/

Original format is a 2D table with years as rows and months as columns. This
script converts it to a 1D table with a single CPI value for each month.

"""

import csv

flat_cpis = []

with open("resources/CPI-box.csv") as f:
    reader = csv.reader(f)
    header = next(reader)
    for row in reader:
        year = row[0]
        month_cpis = row[1:13]
        for month, cpi in enumerate(month_cpis):
            flat_cpis.append([
                f"{year}-{month+1:02d}", cpi
            ])

with open("resources/CPI.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(["%Y-%m", "CPI"])
    writer.writerows(flat_cpis)
