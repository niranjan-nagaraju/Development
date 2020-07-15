'''
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
'''
import numpy as np
class LCS(object):
	def __init__(self, A, B):
		self.A = A
		self.B = B
		self.lcs_table = None


	# Build an LCS DP table
	def make_lcs_table(self):
		m = len(self.A)
		n = len(self.B)
		T = [[0 for _ in xrange(n+1)] for _ in xrange(m+1)]

		for i in xrange(1, m+1):
			for j in xrange(1, n+1):
				if self.A[i-1] == self.B[j-1]:
					T[i][j] = 1 + T[i-1][j-1]
				else:
					T[i][j] = max(T[i-1][j], T[i][j-1])
		#print np.array(T)
		self.lcs_table = T


	# Find LCS length from the DP table
	def find_lcs_length(self):
		if not self.lcs_table:
			self.make_lcs_table()
		return self.lcs_table[-1][-1]


	# Retrace DP table to get one of the longest common subsequence
	def find_lcs_sequence(self):
		if not self.lcs_table:
			self.make_lcs_table()
		lcs_len = self.lcs_table[-1][-1]

		i = len(self.A)
		j = len(self.B)
		lcs_seq = [0]*lcs_len

		while i and j:
			if self.A[i-1] == self.B[j-1]:
				lcs_seq[lcs_len-1] = self.A[i-1]
				lcs_len -= 1
				i -= 1
				j -= 1
			elif self.lcs_table[i-1][j] > self.lcs_table[i][j-1]:
				i -= 1
			else: # self.lcs_table[i-1][j] <= self.lcs_table[i][j-1]:
				j -= 1

		return ''.join(lcs_seq)



if __name__ == '__main__':
	assert LCS("GXTXAYB", "AGGTAB").find_lcs_length() == 4

	assert LCS("abbcdgf", "bbadcgf").find_lcs_length() == 5
	assert LCS("abbcdgf", "bbadcgf").find_lcs_sequence() == "bbdgf"

	assert LCS("ABCDGH", "AEDFHR").find_lcs_length() == 3
	assert LCS("ABCDGH", "AEDFHR").find_lcs_sequence() == "ADH"

	l = LCS("GXTXAYB", "AGGTAB")
	assert l.find_lcs_length() == 4
	assert l.find_lcs_sequence() == "GTAB"

