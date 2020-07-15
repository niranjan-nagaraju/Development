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
	1. Start with two pointers, one at the start, and the other at the end of the string.
		1.1 If A[0] == A[n-1]
				Add 2 to the longest palindromic subsequence length (lps_l), and compute lps_l(A[1..n-2])
			Otherwise,
				lps_l(A[0..n-1]) = max(lps_l(A[0..n-2]), lps(A[1..n-1]))
				lps_l(A[i..i]) = 1 -- each letter on its own is a palindrome
				lps_l([]) == 0
	2. Generalizing 1.1 for i,j (i<=j)
		lps_l(A[i..j]) = {
							1 if i == j
							2+lps_l(A[i+1 .. j-1]) if A[i] == A[j]
							max(
								lps_l(A[i+1 .. j),
								lps_l(A[i .. j-1)
								) otherwise
						}
	3. Building a DP table T[][] of size n x n,
		T[i][j] = lps_l(A[i..j])
		=>
		T[i][i] = 1 (0 <= i < n)
		T[i][j] = T[i+1][j-1] + 2 if A[i] == A[j] otherwise max(T[i+1][j], T[i][j-1])
	4. Build the DP table bottom-up starting T[i][i] (palindromes of length 1)
		and then T[i][i+1], palindromes of length 2,
		T[i][i+2], ...
		T[0][n-1] contains lps_l(A)


Sample run:
	A: "bebeeed"
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b |   |   |   |   |   |   |   |
	| e |   |   |   |   |   |   |   |
	| b |   |   |   |   |   |   |   |
	| e |   |   |   |   |   |   |   |
	| e |   |   |   |   |   |   |   |
	| e |   |   |   |   |   |   |   |
	| d |   |   |   |   |   |   |   |


	Palindromes in Array subsets of length 1:
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b | 1 |   |   |   |   |   |   |
	| e |   | 1 |   |   |   |   |   |
	| b |   |   | 1 |   |   |   |   |
	| e |   |   |   | 1 |   |   |   |
	| e |   |   |   |   | 1 |   |   |
	| e |   |   |   |   |   | 1 |   |
	| d |   |   |   |   |   |   | 1 |

	Palindromes in Array subsets of length 2:
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b | 1 | 1 |   |   |   |   |   |
	| e |   | 1 | 1 |   |   |   |   |
	| b |   |   | 1 | 1 |   |   |   |
	| e |   |   |   | 1 | 2 |   |   |
	| e |   |   |   |   | 1 | 2 |   |
	| e |   |   |   |   |   | 1 | 1 |
	| d |   |   |   |   |   |   | 1 |

	Palindromes in Array subsets of length 3:
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b | 1 | 1 | 3 |   |   |   |   |
	| e |   | 1 | 1 | 3 |   |   |   |
	| b |   |   | 1 | 1 | 2 |   |   |
	| e |   |   |   | 1 | 2 | 3 |   |
	| e |   |   |   |   | 1 | 2 | 2 |
	| e |   |   |   |   |   | 1 | 1 |
	| d |   |   |   |   |   |   | 1 |

	Palindromes in Array subsets of length 4:
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b | 1 | 1 | 3 | 3 |   |   |   |
	| e |   | 1 | 1 | 3 | 3 |   |   |
	| b |   |   | 1 | 1 | 2 | 3 |   |
	| e |   |   |   | 1 | 2 | 3 | 3 |
	| e |   |   |   |   | 1 | 2 | 2 |
	| e |   |   |   |   |   | 1 | 1 |
	| d |   |   |   |   |   |   | 1 |

	Palindromes in Array subsets of length 5:
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b | 1 | 1 | 3 | 3 | 3 |   |   |
	| e |   | 1 | 1 | 3 | 3 | 4 |   |
	| b |   |   | 1 | 1 | 2 | 3 | 3 |
	| e |   |   |   | 1 | 2 | 3 | 3 |
	| e |   |   |   |   | 1 | 2 | 2 |
	| e |   |   |   |   |   | 1 | 1 |
	| d |   |   |   |   |   |   | 1 |

	Palindromes in Array subsets of length 6:
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b | 1 | 1 | 3 | 3 | 3 | 4 |   |
	| e |   | 1 | 1 | 3 | 3 | 4 | 4 |
	| b |   |   | 1 | 1 | 2 | 3 | 3 |
	| e |   |   |   | 1 | 2 | 3 | 3 |
	| e |   |   |   |   | 1 | 2 | 2 |
	| e |   |   |   |   |   | 1 | 1 |
	| d |   |   |   |   |   |   | 1 |

	Palindromes in Array subsets of length 7:
	T:
	|   | b | e | b | e | e | e | d |
	|---+---+---+---+---+---+---+---|
	| b | 1 | 1 | 3 | 3 | 3 | 4 | 4 |
	| e |   | 1 | 1 | 3 | 3 | 4 | 4 |
	| b |   |   | 1 | 1 | 2 | 3 | 3 |
	| e |   |   |   | 1 | 2 | 3 | 3 |
	| e |   |   |   |   | 1 | 2 | 2 |
	| e |   |   |   |   |   | 1 | 1 |
	| d |   |   |   |   |   |   | 1 |

	T[0][-1] == 4	which represents "eeee"
'''
import numpy as np
class Solution:
	def find_lps_length(self, A):
		if not A:
			return 0

		n = len(A)
		T = [[1 if (i==j) else 0  for j in xrange(n)] for i in xrange(n)]

		'''
		n = 7
		(0, 1) (1, 2) (2, 3) (3, 4) (4, 5) (5, 6)
		(0, 2) (1, 3) (2, 4) (3, 5) (4, 6)
		(0, 3) (1, 4) (2, 5) (3, 6)
		(0, 4) (1, 5) (2, 6)
		(0, 5) (1, 6)
		(0, 6)
		'''
		for k in xrange(1, n):
			for i in xrange(0, n-k):
				j = i+k
				if A[i] == A[j]:
					T[i][j] = 2 + T[i+1][j-1]
				else:
					T[i][j] = max(T[i+1][j], T[i][j-1])

		#print np.array(T)
		return T[0][-1]


if __name__ == '__main__':
	s = Solution()
	assert s.find_lps_length("bebeeed") == 4
	assert s.find_lps_length("balbed") == 3
	assert s.find_lps_length("bbbab") == 4
	assert s.find_lps_length("cbbd") == 2
	assert s.find_lps_length("underqualified") == 7 #deified
	assert s.find_lps_length("turboventilator") == 7 # rotator
	assert s.find_lps_length("r1a2c3e4c5a6r7") == 7 # racecar


