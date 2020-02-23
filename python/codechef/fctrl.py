def trailingZeroes (n):
		divisor = 1
		numZeroes = 0
		while (n >= divisor*5):
				numZeroes = numZeroes + (n/(divisor *5))
				divisor = divisor * 5
		return numZeroes


inputList = []
numInput = int(input())
while (numInput > 0):
	number = int(input())
	inputList.append(number)
	numInput = numInput - 1

for i in inputList:
	print trailingZeroes(i)
