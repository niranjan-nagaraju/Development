'''
https://www.interviewbit.com/problems/longest-common-prefix/

Longest Common Prefix

Given the array of strings A,
you need to find the longest string S which is the prefix of ALL the strings in the array.

Longest common prefix for a pair of strings S1 and S2 is the longest string S which is the prefix of both S1
and S2.

For Example, longest common prefix of "abcdefgh" and "abcefgh" is "abc".

nput Format

The only argument given is an array of strings A.
Output Format

Return longest common prefix of all strings in A.
For Example

Input 1:
    A = ["abcdefgh", "aefghijk", "abcefgh"]
Output 1:
    "a"
Explanation 1:
	Longest common prefix of all the strings is "a".

Input 2:
    A = ["abab", "ab", "abcd"];
Output 2:
    "ab"
Explanation 2:
	Longest common prefix of all the strings is "ab".
'''

'''
Solution Outline:
	Observations: The longest common prefix only decreases(/or stays the same) with the introduction of each new word, never increases.
	E.g, 
	  w1: "abcde" , lcp initially is 5
	  w2: "abcd", lcp is now 4

	This is true regardless of the order in which the words arrive.
	  w1: "abcd" , lcp initially is 4
	  w2: "abcde", lcp stays at 4

	1. Stack the strings one below another, and compare columns
	2. Copy each column value which remains the same for all strings from the left
	3. Return when any of the strings at any column, i, differs from its predecessors' column

Sample run:
	words: ["abc", "abcdef", "abxif"]

	columns  0 1 2 3 4 5
	w0       a b c
	w1       a b c d e f
	w2       a b x i f

Column 2 differs for w2, break and return 2
'''
class Solution:
	def find_longest_common_prefix(self, A):
		if len(A) == 0 or len(A[0]) == 0:
			return 0

		lcp = []

		# Loop till min(any word length)
		# A[0] is atleast >= min
		for column in xrange(len(A[0])):
			c = A[0][column]
			for i in xrange(1, len(A)):
				if column >= len(A[i]) or A[i][column] != c:
					return ''.join(lcp)

			# all strings have the same matching character in 'column'
			lcp.append(c)

		return ''.join(lcp)





if __name__ == '__main__':
	s = Solution()
	assert s.find_longest_common_prefix(["abcd", "abce", "abxf"]) == "ab"
	assert s.find_longest_common_prefix(["abxf", "abcd", "abce"]) == "ab"
	assert s.find_longest_common_prefix(["abcd", "abce", "axf"]) == "a"
	assert s.find_longest_common_prefix(["abcd", "abce", "f"]) == ""
	assert s.find_longest_common_prefix(["abcd", "abce", ""]) == ""
	assert s.find_longest_common_prefix(["abcd", "abcde", ""]) == ""

