'''
https://www.interviewbit.com/problems/implement-strstr/

Implement StrStr

Please Note:
Another question which belongs to the category of questions which are intentionally stated vaguely.
Expectation is that you will ask for correct clarification or you will state your assumptions before you start coding.

Implement strStr().
	strstr - locate a substring ( needle ) in a string ( haystack ). 

Try not to use standard library string functions for this question.
Returns the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.

NOTE: Good clarification questions:
	What should be the return value if the needle is empty?
	What if both haystack and needle are empty?
For the purpose of this problem, assume that the return value should be -1 in both cases. 
'''

'''
Solution Outline:
	Use Knuth-Morris-Pratt;s algorithm to find B in A
'''
class Solution:
	# Find B in A
	# @param A : string
	# @param B : string
	# @return an integer
	def strStr(self, A, B):
		# B is the needle
		# A is the haystack

		# Pre-compute LPS array for the pattern
		def compute_lps(pat, m):
			lp = 0
			lps = [0] * m
			i = 1
			while i < m:
				if pat[i] == pat[lp]:
					lp += 1
					lps[i] = lp
					i += 1
				else:
					if lp == 0:
						lps[i] = 0
						i += 1
					else:
						lp = lps[lp-1]
			return lps

		if not A or not B:
			return -1

		i = j = 0
		m = len(B)
		n = len(A)

		lps = compute_lps(B, m)
		while i < n:
			if A[i] == B[j]:
				i += 1
				j += 1

			# Matched the whole substring
			if j == m:
				return i-j

			elif i<n and A[i] != B[j]:
				if j == 0:
					i += 1
				else:
					# Backtrack to see if there are other prefixes in pattern, B, that
					# end with text, A[i]
					j = lps[j-1]

		return -1




if __name__ == '__main__':
	s = Solution()
	assert s.strStr("HelloWorld", "Hello") == 0
	assert s.strStr("HelloWorld", "World") == 5
	assert s.strStr("ABCHellHelloWorld", "Hello") == 7

