#!/usr/local/bin/python3

# 'yield from' works only in python3
def getN(n):
	if n == 0:
		return

	yield n
	yield from getN(n-1)

for x in getN(5):
	print(x)

