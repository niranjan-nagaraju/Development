#!/usr/bin/python

import sys

def sequence(n, cyclecount):
	if n == 1:
		return cyclecount + 1

	if n & 1 == 1:
		currCnt = sequence((3 * n + 1), cyclecount+1)
	else:
		currCnt = sequence(n / 2, cyclecount+1)

	return currCnt

def findMaxCycleCount(i, j):
	maxCnt = 0
	for x in range(i, j+1):
		currCnt = sequence(x, 0)
		if (currCnt > maxCnt):
			maxCnt = currCnt

	return maxCnt

inputList = []
results = []
for line in sys.stdin.readlines():
	strLine = line[:-1].split(" ")
	i, j = int(strLine[0]), int(strLine[1])
	results.append([i,j,findMaxCycleCount(i,j)])

for result in results:
	print result[0], result[1], result[2]
