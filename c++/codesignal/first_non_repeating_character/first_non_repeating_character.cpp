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
Solution Outline:
	Use a hash-table, and keep a count of the characters encountered.
    scan the array again
	  for the one with an occurence count of 1
*/

#include<iostream>
#include<assert.h>
#include<unordered_map>

char firstNotRepeatingCharacter(std::string s) {
	std::unordered_map<char, int>occurences;

	for (char c : s) {
		 if (occurences.find(c) == occurences.end())
			 occurences[c] = 0;
		 occurences[c]++;
	}

	for (char c : s) {
		if (occurences[c] == 1)
			return c;
	}

	return '_';
}


int main(void) {
	assert (firstNotRepeatingCharacter("abacabad") == 'c');
	assert (firstNotRepeatingCharacter("abacabaabacaba") == '_');

	return 0;
}

