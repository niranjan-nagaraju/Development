#!/bin/python

#Which prime, below one-million, can be written as the sum of the most consecutive primes?

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

def maxConsecutiveSum (list):
	currsum = 0
	maxsum = 0
	for i in list:
		# if current element increases our current max sum, include it into the list
		if (currsum + i) > currsum:
			currsum = currsum + i
			if maxsum < currsum:
				maxsum = currsum # we found a new set that beats the prev maximum
		else:
			currsum = 0  # start over to find new maximum sum
		#print "Processing element: ", i, "Maxsum: ", maxsum, "Currsum: ", currsum
	
	return maxsum



L = sieve(1000000)
print maxConsecutiveSum (L)
