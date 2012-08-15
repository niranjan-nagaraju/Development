#!/usr/bin/python

# Recursive helper to convert a number to string
def intToStr_h (x, strList):
	if (x == 0):
		return

	intToStr_h (x/10, strList)
	strList += [chr((x%10) + ord('0'))]


# Handle +ve and -ve numbers
def intToStr (x):
	string = []

	# if the number is -ve, make it +ve, but store the sign in the string
	if x < 0:
		x = -x
		string += '-'

	intToStr_h (x, string)
	return "".join(string)


def main():
	print intToStr(1234), intToStr(-12)

if __name__ == "__main__":
	main()
