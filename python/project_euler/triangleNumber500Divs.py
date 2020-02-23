#!/usr/bin/python

''' Project Euler - Problem 12 '''

def divisors(n):
	if (n == 1):
		return [1]

	divs = []
	for i in range(1, n/2+1):
		if ( n % i == 0):
			divs.append(i)

	divs.append(n)
	return divs

def triangleNumbers(dc):
	tn = 41664756 
	j = 9129
	while True:
		tn = tn + j
		j = j + 1
		divs = divisors(tn)
		ld = len(divs)
		print tn, ld
		if ld >= dc:
			return [tn, ld]

print 'Result ', triangleNumbers(500)
