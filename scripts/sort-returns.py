"""

sort-returns.py
---------------

Author: Michael Dickens <mdickens93@gmail.com>
Created: 2015-12-22

"""

def read_org_line(line):
    entries = [ s.strip() for s in line.split("|") ]
    del entries[0]
    cols = "name adj return risk negative underperform".split()
    return dict(zip(cols, entries))

def print_org_line(row):
    names = [ x.strip() for x in row['name'].split(',') ]
    if names[0] > names[1]:
        return None
    print "| %s | %s | %s | %s | %s | %s |" % (row['name'], row['adj'], row['return'], row['risk'], row['negative'], row['underperform'])

def exec_file(filename):
    rows = []
    with open(filename, 'r') as fp:
        for line in fp:
            rows.append(read_org_line(line))

    rows.sort(key=lambda r: r['adj'])
    for row in rows:
        print_org_line(row)

exec_file("returns-unsorted.org")
