#!/bin/python

#Sieve of Erathostenes

from math import sqrt

def sieve(n):
	#A = range(1,(n+1)) # A[i] = i, forall i
	i = 0
	A = []
	while i<=n:
		A.append(i)
		i = i+1
	for p in range(2, int(sqrt(n))):
		if A[p] != 0: # p hasn't been eliminated before
			j = p * p
			while  j<= n:
				A[j] = 0
				j = j + p
	
	# Copy the remaining elements to the List of primes
	L = []
	for p in A[2:]:
		if (p != 0):
			L.append(p)

	return L


L = sieve(1000000)
# print len(L)
n = 10000
print 'Prime(', n, '):', L[n]
