#!/usr/bin/python

def getTriangle(nr):
	inTriangle = []
	while ( nr > 0 ):
		currRowStr = raw_input()
		currRowStr = currRowStr.strip()
		currRow = map (int, (currRowStr.split(' ')))
		inTriangle.append(currRow)
		nr = nr - 1
	return inTriangle


def getCandiates(currRow):
	candidates = []	

	rlen = len(currRow)
	for i in range (0, rlen-1):
		candidates.append(max(currRow[i], currRow[i+1]))

	return candidates


def reduceSum(prevRow, candidates):
	rlen = len(prevRow) # same as candidates list
	
	for i in range(0, rlen):
		prevRow[i] += candidates[i]

def calculateMaxTriangleSum(numTriangle):
	tlen = len(numTriangle)
	for i in range (tlen-1, 0, -1):
		currRow = numTriangle[i]
		prevRow = numTriangle[i-1]
		candidates = getCandiates(currRow)
		reduceSum(prevRow, candidates)
	
	return numTriangle[0][0]

def getInput():
	numCases = int(input())
	while (numCases != 0):
		nr = int(input())
		numTriangle = getTriangle(nr)
		print calculateMaxTriangleSum(numTriangle)
		numCases = numCases - 1

getInput()


