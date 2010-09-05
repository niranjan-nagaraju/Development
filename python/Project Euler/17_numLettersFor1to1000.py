numToLetters = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]

numToLettersTens = ["", "Ten", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety", "Hundred"]

def numberToLetter(n):
	thousands = 0
	hundreds = 0
	tens = 0

	numString = ""

	# The thousands
	thousands = n / 1000
	n %= 1000
	if (thousands != 0):
		numString += numToLetters[thousands] + "Thousand"

	# The hundreds
	hundreds = n / 100
	n %= 100
	if (hundreds != 0):
		numString += numToLetters[hundreds] + "Hundred"
		if ( n != 0): # A full hundred, don't include And, Otherwise do
			numString += "And"

	# Tens, but handle all cases <20 directly
	if ( n < 20 ):
		numString += numToLetters[n]
		return numString

	# Tens > 20
	tens = n / 10
	n %= 10
	if (tens != 0):
		numString += numToLettersTens[tens]
	
	# now the ones
	if (n != 0):
		numString += numToLetters[n]

	
	return numString

'''
print numberToLetter(342)
print numberToLetter(1342)
print numberToLetter(12)
print numberToLetter(302)
print numberToLetter(300)
print numberToLetter(20)
print numberToLetter(320)
'''

numLetters = 0
for i in range(1, 1001):
	numLetters += len(numberToLetter(i))

print numLetters
