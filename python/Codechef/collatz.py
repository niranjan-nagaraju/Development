#!/usr/bin/python

def collatz(n):
	len = 0
	while n != 1:
		if (n & 1):
			n = 3*n + 1
		else:
			n = n / 2
		len = len + 1
	return len+1

max = 0
for i in range(1,1000001):
	num = collatz(i)
	if (num > max):
 		max = num
		maxnum = i
#	print i, num, max, maxnum
print max, maxnum
