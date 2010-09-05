#!/usr/bin/python

def sumOfProperDivisiors(n):
	divisor = 1
	sum = 0
	while (divisor != n):
		if ( n % divisor == 0):
			sum = sum + divisor
		divisor = divisor + 1
	return sum


for i in range(2,10000):
	pairI = sumOfProperDivisiors(i)
	if (sumOfProperDivisiors(pairI) == i and (pairI != i)):
		print pairI, i


