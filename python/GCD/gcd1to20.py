#!/bin/python

def gcd(m, n):
		if n == 0:
				return m
		else:
				return gcd(n,m%n)

x = 1
intergcd = 1
while (x <= 20):
	intergcd = gcd(x, intergcd)
	print x,intergcd
	x = x + 1

print intergcd

