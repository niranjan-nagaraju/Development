#!/usr/bin/python

from Primes1mil import *

def rotateList (num):
	numStr = str(num)
	digits = len(numStr)

	rotatedList = []	
	for i in range(1, digits):
		rotatednumStr = numStr[1:] + numStr[:1]
		numStr = rotatednumStr

		rotatednum = int(rotatednumStr)
		if (len(str(rotatednum)) != digits):
			return []
		else:
			rotatedList.append(int(rotatednumStr))
	
	return rotatedList

def isCircularPrime (num):
	rotatedList = rotateList(num)
	if rotatedList != []:
		return reduce (lambda x, y: x and (y in primes), rotateList(num), True)

	return False


def main():
	circularPrimes = [x for x in primes if isCircularPrime(x)]
	print circularPrimes
	print len(circularPrimes)
	
if __name__ == "__main__":
	main()

'''
[11, 13, 17, 31, 37, 71, 73, 79, 97, 113, 131, 197, 199, 311, 337, 373, 719, 733, 919, 971, 991, 1193, 1931, 3119, 3779, 7793, 7937, 9311, 9377, 11939, 19391, 19937, 37199, 39119, 71993, 91193, 93719, 93911, 99371, 193939, 199933, 319993, 331999, 391939, 393919, 919393, 933199, 939193, 939391, 993319, 999331]
51
'''
