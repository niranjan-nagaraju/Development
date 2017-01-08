#!/usr/bin/python

def sum_of_divisors(n):
	prod = 1
	k = 2
	while (k*k <= n):
		p = 1
		while (n % k == 0):
			p = p * k + 1
			n /= k
		prod *= p
		k += 1

	if n > 1:
		prod *= 1+n

	return prod

print sum_of_divisors(24)

'''
n = p1 ** a1 * p2 ** a2 ...
then
sum_of_divisors_including_n = (1+ p1 + p1**2 + .. + p1**a1) * (1+ p2 + .. p2**a2) * ...
'''
