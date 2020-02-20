/**
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
*/



/**
Solution Outline: O(n) time, O(1) memory
	Use a lookup table of 26 characters (as input set is lowercase alphabets), and keep a count of the characters encountered.
    scan the array again
	  for the one with an occurence count of 1
*/

#include<iostream>
#include<assert.h>


#define NUM_CHARS 26 // Input set is lowercase alphabets

char firstNotRepeatingCharacter(std::string s) {
	char occurences[NUM_CHARS] = {0};

	for (char c : s) {
	 occurences[c-'a']++;
	}

	for (char c : s) {
		if (occurences[c-'a'] == 1)
			return c;
	}

	return '_';
}


int main(void) {
	assert (firstNotRepeatingCharacter("abacabad") == 'c');
	assert (firstNotRepeatingCharacter("abacabaabacaba") == '_');

	return 0;
}

