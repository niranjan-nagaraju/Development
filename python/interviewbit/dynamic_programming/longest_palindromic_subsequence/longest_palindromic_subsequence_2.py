'''
https://www.interviewbit.com/problems/longest-palindromic-subsequence/

Longest Palindromic Subsequence

Problem Description
Given a string A, find the common palindromic sequence ( A sequence which does not need to be contiguous and is a palindrome), which is common in itself.
You need to return the length of longest palindromic subsequence in A.

NOTE:
Your code will be run on multiple test cases (<=10). Try to come up with an optimised solution.


Problem Constraints
1 <= |A| <= 1005


Input Format
First and only argument is an string A.

Output Format
Return a integer denoting the length of longest palindromic subsequence in A.

Example Input
Input 1:
 A = "bebeeed"

Example Output
Output 1:
 4

Example Explanation
Explanation 1:
 The longest common palindromic subsequence is "eeee", which has a length of 4
'''

'''
Solution Outline:
	1. Longest palindromic subsequence of a given array A, is the same as
		longest common subsequence (A, A')
			where A': reverse(A)
		For e.g.,
			A:  "balbed"
			A': "deblab"
			LCS(A, A'): "bab"/"blb"

Sample run:
	A:  "bebeeed"
	A': "deeebeb"
	T:
	|   |   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---+---|
	|   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
	| d | 0 |   |   |   |   |   |   |   |
	| e | 0 |   |   |   |   |   |   |   |
	| e | 0 |   |   |   |   |   |   |   |
	| e | 0 |   |   |   |   |   |   |   |
	| b | 0 |   |   |   |   |   |   |   |
	| e | 0 |   |   |   |   |   |   |   |
	| b | 0 |   |   |   |   |   |   |   |

	Fill LCS DP
	T:
	|   |   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---+---|
	|   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
	| d | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 |
	| e | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 1 |
	| e | 0 | 0 | 1 | 1 | 2 | 2 | 2 | 2 |
	| e | 0 | 0 | 1 | 1 | 2 | 3 | 3 | 3 |
	| b | 0 | 1 | 1 | 2 | 2 | 3 | 3 | 3 |
	| e | 0 | 1 | 2 | 2 | 3 | 3 | 4 | 4 |
	| b | 0 | 1 | 2 | 3 | 3 | 3 | 4 | 4 |

T[-1][-1] = 4 = LPS_l("bebeeed")
'''
import numpy as np
class Solution:
	def find_lps_length(self, A):
		if not A:
			return 0

		n = len(A)

		# LCS only requires the previous-row and the current-row for processing
		# Store only two rows and alternate updates between them
		T = [[0  for j in xrange(n+1)] for i in xrange(2)]

		A_ = A[::-1]
		for i in xrange(1, n+1):
			for j in xrange(1, n+1):
				T[(i&1)][j] = (1+T[(i-1)&1][j-1]) if (A[i-1] == A_[j-1]) else \
						(max(T[(i-1)&1][j], T[i&1][j-1]))

		#print np.array(T)
		return T[n&1][-1]


if __name__ == '__main__':
	s = Solution()
	assert s.find_lps_length("bebeeed") == 4
	assert s.find_lps_length("bbbab") == 4
	assert s.find_lps_length("cbbd") == 2
	assert s.find_lps_length("underqualified") == 7 #deified
	assert s.find_lps_length("turboventilator") == 7 # rotator
	assert s.find_lps_length("r1a2c3e4c5a6r7") == 7 # racecar


