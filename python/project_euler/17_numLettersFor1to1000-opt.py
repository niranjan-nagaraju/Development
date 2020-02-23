'''
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
'''


OneToNine = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]

ElevenToNineteen = ["Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]

Tens = ["", "Ten", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]

def countLettersInList(numList):
	count = 0
	for n in numList:
		count = count + len(n)

	return count

def countLettersTillNine():
	return countLettersInList(OneToNine)

def countLettersElevenTillNineteen():
	return countLettersInList(ElevenToNineteen)


def countLettersTillNinetyNine():
	count = countLettersTillNine()
	count = count + len(Tens[1]) + countLettersElevenTillNineteen()

	# 21-29, 31-39, ..., 91-99
	count = count + 10 * countLettersInList(Tens[2:])
	count = count +  8 * countLettersInList(OneToNine)

	return count


def countLettersTillThousand():
	count = countLettersTillNinetyNine()
	count = count + countLettersTillNine() * 100  # One, Two, ..., Nine
	count = count + len("Hundred") * 900 + len("AND") * 891  # Hundred AND ['AND' will be missing for whole 100, 200, etc]
	count = count + countLettersTillNinetyNine() * 9  # One, Two, ..., NinetyNine

	count = count + len("One") + len("Thousand")

	return count

print countLettersTillThousand()

# Solution: 21124
