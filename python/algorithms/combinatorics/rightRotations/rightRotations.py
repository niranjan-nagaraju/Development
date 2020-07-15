#!/usr/bin/python

# Print out all rotations of a number

def rightRotations (n):
	rotations = []
	nStr = str(n)
	rotations.append(n)
	for i in xrange(1,len(nStr)):
		nStr = nStr[-1] + nStr[:-1]
		rotations.append(int(nStr))

	return rotations



if __name__ == '__main__':
	assert rightRotations(123) == [123, 312, 231]

