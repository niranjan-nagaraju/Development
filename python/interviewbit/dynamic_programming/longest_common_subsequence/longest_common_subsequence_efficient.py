'''
https://www.interviewbit.com/problems/longest-common-subsequence/

Longest Common Subsequence

Problem Description
Given two strings A and B. Find the longest common sequence ( A sequence which does not need to be contiguous), which is common in both the strings.
You need to return the length of such longest common subsequence.

Problem Constraints
1 <= |A|, |B| <= 1005

Input Format
First argument is an string A.
Second argument is an string B.

Output Format
Return the length of such longest common subsequence between string A and string B.


Example Input
Input 1:
 A = "abbcdgf"
 B = "bbadcgf"


Example Output
Output 1:
 5

Example Explanation
Explanation 1:
 The longest common subsequence is "bbcgf", which has a length of 5
'''

'''
Solution Outline:
	0. Let m be the length of A, n be the length of B
	1. Let x be the length of the longest common subsequence(LCS_l) of A[0:m-2], and B[0:n-2]
		Then, including the last character of A and B,
			if A[-1] == B[-1],
				Then add +1 to LCS len of A[0:m] and B[0:n], => x+1
			otherwise,
				LCS length would be max(LCS_l(A[0:m-1], B[0:n-2]), LCS_l(A[0:m-2], B[0:n-1])
	2. Generalizing to i,j
		LCS_l(A[0:i], B[0:j]) == max {
										LCS_l(A[0:i-1], B[0:j]),
										LCS_l(A[0:i], B[0:j-1])
									} if A[i] != B[j]
							== 1+LCS_l(A[0:i-1], B[0:j-1]) if A[i] == B[j]
	3. Trivial case:
		LCS_l([], []) = 0
		LCS_l(A, []) = 0
		LCS_l([], []) = 0
	4. Fill a bottom-up DP table, T[][], of m x n, where T[i][j] = LCS_l(A[0..i], B[0..j])
		T[0][j] = 0 for all 0 <= j <= n
		T[i][0] = 0 for all 0 <= i <= m
		T[m][n] will be the LCS_l(A, B)

Sample run:
	A: "AGGTAB"
	B: "GXTXAYB"

	T:
	|   |   | A | G | G | T | A | B |
	|---+---+---+---+---+---+---+---|
	|   | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
	| G | 0 |   |   |   |   |   |   |
	| X | 0 |   |   |   |   |   |   |
	| T | 0 |   |   |   |   |   |   |
	| X | 0 |   |   |   |   |   |   |
	| A | 0 |   |   |   |   |   |   |
	| Y | 0 |   |   |   |   |   |   |
	| B | 0 |   |   |   |   |   |   |

	T: "G" vs "AGGTAB"
	|   |   | A | G | G | T | A | B |
	|---+---+---+---+---+---+---+---|
	|   | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
	| G | 0 | 0 | 1 | 1 | 1 | 1 | 1 |
	| X | 0 |   |   |   |   |   |   |
	| T | 0 |   |   |   |   |   |   |
	| X | 0 |   |   |   |   |   |   |
	| A | 0 |   |   |   |   |   |   |
	| Y | 0 |   |   |   |   |   |   |
	| B | 0 |   |   |   |   |   |   |

	T: "GX" vs "AGGTAB"
	|   |   | A | G | G | T | A | B |
	|---+---+---+---+---+---+---+---|
	|   | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
	| G | 0 | 0 | 1 | 1 | 1 | 1 | 1 |
	| X | 0 | 0 | 1 | 1 | 1 | 1 | 1 |
	| T | 0 |   |   |   |   |   |   |
	| X | 0 |   |   |   |   |   |   |
	| A | 0 |   |   |   |   |   |   |
	| Y | 0 |   |   |   |   |   |   |
	| B | 0 |   |   |   |   |   |   |

	T: {Rest of the table}
	|   |   | A | G | G | T | A | B |
	|---+---+---+---+---+---+---+---|
	|   | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
	| G | 0 | 0 | 1 | 1 | 1 | 1 | 1 |
	| X | 0 | 0 | 1 | 1 | 1 | 1 | 1 |
	| T | 0 | 0 | 1 | 1 | 2 | 2 | 2 |
	| X | 0 | 0 | 1 | 1 | 2 | 2 | 2 |
	| A | 0 | 1 | 1 | 1 | 2 | 3 | 3 |
	| Y | 0 | 1 | 1 | 1 | 2 | 3 | 3 |
	| B | 0 | 1 | 1 | 1 | 1 | 3 | 4 |

NOTE: T[i][j] is dependent on current row and the row before that
		so all we need are 2 rows at any given time.
'''
import numpy as np
class Solution:
	def find_lcs_len(self, A, B):
		m = len(A)
		n = len(B)
		T = [[0 for _ in xrange(n+1)] for _ in xrange(2)]

		for i in xrange(1, m+1):
			for j in xrange(1, n+1):
				if A[i-1] == B[j-1]:
					T[i&1][j] = 1 + T[(i-1)&1][j-1]
				else:
					T[i&1][j] = max(T[(i-1)&1][j], T[i&1][j-1])
		print np.array(T)
		return T[m&1][-1]


if __name__ == '__main__':
	s = Solution()
	assert s.find_lcs_len("GXTXAYB", "AGGTAB") == 4
	assert s.find_lcs_len("abbcdgf", "bbadcgf") == 5
	assert s.find_lcs_len("ABCDGH", "AEDFHR") == 3

