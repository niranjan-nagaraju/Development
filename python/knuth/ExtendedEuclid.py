#!/usr/bin/python

''' 
Given m & n, find GCD d and integers a & b s.t.
	am + bn = d
'''

def extendedEuclid (m, n):
	aa = b = 1
	a = bb = 0
	c = m
	d = n

	q = c / d
	r = c % d

	while r != 0:
		c = d
		d = r

		'''t = aa
		aa = a
		a = t - q*a'''

		(aa, a) = (a, aa-q*a)
		
		'''t = bb
		bb = b
		b = t - q*b'''
		(bb, b) = (b, bb-q*b)

		print a, aa, b, bb, c, d, q, r
		
		q = c/d
		r = c%d

	
	return (a, b, d)

print extendedEuclid(1769, 551)

