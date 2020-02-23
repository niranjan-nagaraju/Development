#!/usr/bin/python

def spiralDiagonals (n):
	sumdiags = 1
	diags = [1,1,1,1]
	next_diags = [2, 4, 6, 8]

	while diags[3] != n:
		for i in [0,1,2,3]:
			diags[i] += next_diags[i]
			next_diags[i] += 8
		sumdiags += sum(diags)
	print sumdiags

spiralDiagonals (1001*1001)
# 669171001
