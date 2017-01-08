#!/usr/bin/python

# Replicate each of the current nP(k-1) permutations 'k' times to make room for a new element
def replicate(a, k):
	n = len(a)
	for j in range(1, k):
		for i in range(0, n):
			a.append(a[i][:])

# Insert new 'number' into the current permutation arrangement nP(k-1) one 'slot' at a time
def arrange(a, number, k):
	n = len(a) / k
	for j in range(0, k):
		for i in range (0, n):
			a[j*n+i].insert(j, number)

# Generate all arrangements of elements in list b
def permute_all(b, n):
	permutations = [[]]

	for i in range(1, n+1):
		replicate(permutations, i)
		arrange(permutations, b[i-1], i)

	return permutations

