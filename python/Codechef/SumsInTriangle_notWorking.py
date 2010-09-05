'''
Start from top to bottom, move only DOWN or DOWN-RIGHT
Find largest sum along the path
'''

def getTriangle(nr):
	inTriangle = []
	while ( nr > 0 ):
		currRowStr = raw_input()
		currRowStr = currRowStr.strip()
		currRow = map (int, (currRowStr.split(' ')))
		inTriangle.append(currRow)
		nr = nr - 1
	return inTriangle

def greaterOfTwo(sum, currRow, index):
	print "Comparing ", currRow[index], currRow[index+1]
	if (currRow[index] > currRow[index+1]):
		return (sum + currRow[index], index)
	else:
		return (sum + currRow[index+1], index+1)

def largestSumOfTriangle():
	nr = int(input())
	inTriangle = getTriangle (nr)
	index = 0
	sum = inTriangle[0][0]
	currRow = 1
	while ( nr > currRow ):
		(sum, index) = greaterOfTwo(sum, inTriangle[currRow], index)
		currRow = currRow + 1 # Advance to next row

	return sum

numCases = int(input())
while (numCases != 0):
	print largestSumOfTriangle()
	numCases = numCases - 1
