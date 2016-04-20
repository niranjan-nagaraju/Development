#!/usr/bin/python

'''
Calculate CAGR 

CAGR = ((FV/PV)^(1/n)) - 1

FV: Final Value
PV: Principal
n: number of years
'''

import math

def calculate_cagr(fv, pv, n):
	cagr = math.pow((fv / pv), (1.0/n)) - 1
	cagr = round(cagr, 4) # Round to 4 decimal places so the %age will be atleast 2 decimal places
	return cagr * 100


def testCases():
	assert (calculate_cagr(5000, 1000, 10)) == 17.46

# Run testcases if executed this file is run on its own
if __name__ == "__main__":
	testCases()
