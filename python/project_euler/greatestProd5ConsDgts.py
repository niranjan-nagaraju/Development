#!/usr/bin/python

''' Project Euler - Problem 8'''

def product(lst):
	prod = 1
	for i in lst:
		prod = prod * int(i)
	return prod

def findGreatestProd (lst):
	n = len(lst)
	greatest = 0
	for i in range(0, n-4):
		prod = product(lst[i:(i+5)])
		if prod > greatest:
			greatest = prod

	return greatest

lst = list(raw_input())
print findGreatestProd(lst)
