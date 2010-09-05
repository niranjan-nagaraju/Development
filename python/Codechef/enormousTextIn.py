#!/usr/bin/python

# Return true if n is divisible by k
def isDivisible (num, k):
	if ( num % k == 0):
		return 1

	return 0

def main():
	inStr = raw_input()
	inStr = inStr.strip()
	[n,k] = map(int, inStr.split(' '))
	
	numDivisibles = 0
	i = 0
	while i < n:
		num = int(input())
		numDivisibles += isDivisible(num, k)
		i += 1
	
	print numDivisibles

if __name__ == "__main__":
	main()
