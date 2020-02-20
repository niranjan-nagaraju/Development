#encoding: utf-8
'''
https://app.codesignal.com/interview-practice/task/uX5iLwhc6L5ckSyNC/description

Given a string s consisting of small English letters, find and return the first instance of a non-repeating character in it. If there is no such character, return '_'.

Example
For s = "abacabad", the output should be
firstNotRepeatingCharacter(s) = 'c'.

There are 2 non-repeating characters in the string: 'c' and 'd'. Return c since it appears in the string first.

For s = "abacabaabacaba", the output should be
firstNotRepeatingCharacter(s) = '_'.

There are no characters in this string that do not repeat.

Input/Output
[execution time limit] 4 seconds (py)

[input] string s
A string that contains only lowercase English letters.

Guaranteed constraints:
1 ≤ s.length ≤ 105.

[output] char
The first non-repeating character in s, or '_' if there are no characters that do not repeat.
'''



'''
Solution Outline: O(n) time, O(1) memory
	Use a lookup table of 26 characters (as input set is lowercase alphabets), and keep a count of the characters encountered.
    scan the array again
	  for the one with an occurence count of 1
'''
def firstNotRepeatingCharacter(s):
	chr_idx = lambda c: ord(c) - ord('a')

	occurences = [0]*26 # Input set is lowercase alphabets
	for c in s:
		idx = chr_idx(c)
		occurences[idx] += 1

	for c in s:
		idx = chr_idx(c)
		if occurences[idx] == 1:
			return c

	return '_'




if __name__ == '__main__':
	assert firstNotRepeatingCharacter("abacabad") == 'c'
	assert firstNotRepeatingCharacter("abacabaabacaba") == '_'

