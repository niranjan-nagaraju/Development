'''
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
from algorithms.dynamic_programming.longest_common_subsequence.longest_common_subsequence_dp\
		import LCS

class LPSq(object):
	def __init__(self, A):
		self.A = A
		self.lcs = LCS(A, A[::-1])


	# Find longest palindromic subsequence length using LCS(A, A')
	# A': reverse(A)
	def find_length(self):
		if not self.A:
			return 0

		return self.lcs.find_lcs_length()


	# Find longest palindromic subsequence using LCS(A, A')
	# A': reverse(A)
	def find_sequence(self):
		if not self.A:
			return ""

		return self.lcs.find_lcs_sequence()


if __name__ == '__main__':
	assert LPSq("bebeeed").find_length() == 4
	assert LPSq("bebeeed").find_sequence() == "eeee"

	l = LPSq("bbbab")
	assert l.find_length() == 4
	assert l.find_sequence() == "bbbb"

	assert LPSq("cbbd").find_length() == 2
	assert LPSq("cbbd").find_sequence() == "bb"

	assert LPSq("underqualified").find_length() == 7 #deified
	assert LPSq("underqualified").find_sequence() == "deified"

	assert LPSq("turboventilator").find_length() == 7 # rotator
	assert LPSq("turboventilator").find_sequence() == "rotator"

	assert LPSq("r1a2c34ec5a6r7").find_length() == 7 # racecar
	assert LPSq("r1a2c34ec5a6r7").find_sequence() == "racecar"


