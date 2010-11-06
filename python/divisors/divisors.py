#!/usr/bin/python

import math

def divisors(n):
	divs = []
	for i in range(1, n/2):
		if ( n % i == 0):
			divs.append(i)

	divs.append(n)
	return [len(divs), divs]

print divisors(int(input()))
