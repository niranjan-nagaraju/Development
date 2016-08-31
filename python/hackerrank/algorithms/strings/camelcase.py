'''
https://www.hackerrank.com/challenges/camelcase

Alice wrote a sequence of words in CamelCase as a string of letters, s, having the following properties:

	It is a concatenation of one or more words consisting of English letters.
	All letters in the first word are lowercase.
	For each of the subsequent words, the first letter is uppercase and rest of the letters are lowercase.

	Given s, print the number of words in on a new line.

	Input Format
		A single line containing string s.

	Output Format
		Print the number of words in string s.

	Sample Input
	saveChangesInTheEditor

	Sample Output
	5

'''

s = raw_input().strip()

words = 0
for c in s:
	if (c.isupper()):
		words += 1

print (words + 1) 
