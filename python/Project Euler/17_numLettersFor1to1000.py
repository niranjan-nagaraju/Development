'''
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
'''


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

def countLettersTillThousand():
	numLetters = 0
	for i in range(1, 1001):
		numLetters += len(numberToLetter(i))

	return numLetters



print countLettersTillThousand()

# Solution: 21124


