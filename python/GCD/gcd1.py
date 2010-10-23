#!/bin/python

def gcd(m, n):
		print m, n
		if n == 0:
				return m
		else:
				return gcd(n,m%n)
				

print gcd(36,24)
