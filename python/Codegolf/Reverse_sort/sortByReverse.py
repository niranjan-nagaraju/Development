#!/usr/bin/python

# Reverse first n elements in the list
def reverse(inList, n):
	i = 0
	j = n - 1

	while i < j:
		tmp = inList[i]
		inList[i] = inList[j]
		inList[j] = tmp
		i = i+1; j = j-1

	return inList

# Find the longest decreasing sequence from the start of the list
def findMaxDecSequence(inList):
	i = 0
	ilen = len(inList) - 1
	increasingSeqEnd = 0
	while (i < ilen) and (inList[i] < inList[i+1]) :
		increasingSeqEnd = increasingSeqEnd + 1
		i = i + 1
	
	decreasingSeqEnd = 0
	while (i < ilen) and (inList[i] > inList[i+1]) : 
		decreasingSeqEnd = decreasingSeqEnd + 1
		i = i + 1

	# increasingSeqEnd => Offset where continuously increasing sequence ends
	# decreasingSeqEnd => Offset where continuously decreasing sequence ends
	return [increasingSeqEnd, decreasingSeqEnd] 

def listReversals(inList):
	reversals = []
	while True:
		[inc, dec] = findMaxDecSequence(inList)
		if dec == 0: # No decreasing sequence found, all sorted.. we are done :)
			return reversals
		elif inc == 0: # starts with a decreasing sequence, just straighten it out right away
			reversals.append(dec + 1)
			reverse(inList, (dec + 1))
		else:
			reversals.append(inc + dec + 1)
			reversals.append(dec + 1)
			reversals.append(inc + dec + 1)
		
			reverse(inList, (inc + dec + 1))
			reverse(inList, (dec + 1))
			reverse(inList, (inc + dec + 1))

#inList = [1,2,3,6,5,4,7,9,8]
#inList = [2,3,4,5,1,6,7,8,9]

strList = raw_input()
strList = strList.split(' ')
inList = []

for i in strList:
	inList.append(int(i))

for i in listReversals(inList):
	print i,
