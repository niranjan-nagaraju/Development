#!/usr/bin/python

from Primes1mil import *

def prime_divisors_and_powers (n):
	i = 0

	divisors = []
	while (n > 1):
		nPowers = 0
		# print primes[i], n

		while ( n % primes[i] == 0):
			nPowers += 1
			n /= primes[i]

		if (nPowers > 0):
			divisors.append((primes[i], nPowers))
		i += 1

	return divisors

def valuesFromDivisors (div):
	return (reduce( (lambda x, y: x + (y[0] ** y[1])), div, 0))

def uniqueApowerB (a, b):
	uniq_values = []

	for x in a:
		divisors = prime_divisors_and_powers(x)
		for y in b:
			curr_exp = map( (lambda l: (l[0], (l[1]*y))), divisors)

			if curr_exp not in uniq_values:
				uniq_values.append(curr_exp)
	return uniq_values

def main():
	lst = uniqueApowerB(range(2, 101), range(2, 101))
	print len(lst)
	# 9183

if __name__ == "__main__":
	main()

