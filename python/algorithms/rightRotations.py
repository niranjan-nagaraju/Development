#!/usr/bin/python

# Print out all rotations of a number

def rightRotations (n):
	rotations = []
	nStr = str(n)
	rotations.append(nStr)
	i = 0
	currRotation = nStr;
	while ( i < len(nStr) ):
		nextRotation = currRotation[len(nStr)-1] + 

