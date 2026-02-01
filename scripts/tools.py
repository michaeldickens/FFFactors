"""

tools.py
--------

Author: Michael Dickens <mdickens93@gmail.com>
Created: 2015-12-07

""" 

def read_features_lists(filename):
    with open(filename, "r") as f:
        return [ [ float(s) for s in line.split() ] for line in f ]

def features_to_1D(coefs, feats):
    return sum([ c * f for (c, f) in zip(coefs, feats) ])
