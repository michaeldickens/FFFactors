"""

manage-chesapeake.py
--------------------

Author  : Michael Dickens
Created : 2025-12-17

Initial version written by Claude; it was buggy so I had to fix it.

"""

import csv
import os
import sys


def parse_tsv_file(filepath):
    """Parse a TSV file and return a dictionary mapping date strings to values."""

    # Month name to number mapping
    month_map = {
        "Jan": "01",
        "Feb": "02",
        "Mar": "03",
        "Apr": "04",
        "May": "05",
        "Jun": "06",
        "Jul": "07",
        "Aug": "08",
        "Sep": "09",
        "Oct": "10",
        "Nov": "11",
        "Dec": "12",
    }

    data = {}

    with open(filepath, "r") as infile:
        reader = csv.reader(infile, delimiter="\t")

        # Read header row to get month names
        header = next(reader)
        months = header[1:]  # Skip first column (year label)

        # Process each data row
        for row in reader:
            year = row[0].strip()
            values = row[1:]

            # Create entry for each month
            for month_name, value in zip(months, values):
                if month_name in month_map:
                    clean_value = value.strip().rstrip("%")
                    month_num = month_map[month_name]
                    date_key = f"{year}-{month_num}"
                    data[date_key] = clean_value

    return data


def merge_tsv_files(input_files, output_file):
    """Merge multiple TSV files into a single CSV with dates as rows."""

    # Parse all input files
    all_data = {}
    file_names = []

    for filepath in input_files:
        # Extract column name from filename (remove .tsv extension)
        col_name = os.path.basename(filepath)
        if col_name.endswith(".tsv"):
            col_name = col_name[:-4]

        file_names.append(col_name)
        all_data[col_name] = parse_tsv_file(filepath)

    # Collect all unique dates
    all_dates = set()
    for data in all_data.values():
        all_dates.update(data.keys())

    # Sort dates chronologically
    sorted_dates = sorted(all_dates)

    # Write output CSV
    with open(output_file, "w", newline="") as outfile:
        writer = csv.writer(outfile)

        # Write header
        writer.writerow(["%Y-%m"] + [s.replace("_", " ") for s in file_names])

        # Write data rows
        for date in sorted_dates:
            row = [date]
            for col_name in file_names:
                value = all_data[col_name].get(date, "")
                row.append(value)
            writer.writerow(row)


prefix = "resources/Chesapeake/"
merge_tsv_files([prefix + f for f in ["Diversified-Trend.tsv", "Multi-Asset-Trend.tsv"]], "resources/Chesapeake_Net.csv")
