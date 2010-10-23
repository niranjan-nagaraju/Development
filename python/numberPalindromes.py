#!/usr/bin/python

def reverse(i):
	n = 0
	while i:
		n = (n * 10) + (i%10)
		i = i / 10
	return n

def palindromes(n, odd):
	startrange = 10**(n/2-1)
	for i in range(startrange, startrange*10):
		k = i * 1000;
		r = reverse(i)
		endrange = 1
		if odd:
			endrange = 10
		for j in range(0,endrange):
			print k + (j * 100) + r

digits = int(raw_input())
palindromes(digits, (digits & 1))
