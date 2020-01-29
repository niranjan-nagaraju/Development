'''
https://leetcode.com/problems/longest-palindromic-subsequence/

516. Longest Palindromic Subsequence

Given a string s, find the longest palindromic subsequence's length in s. You may assume that the maximum length of s is 1000.

Example 1:
	Input:

	"bbbab"
	Output:
	4
One possible longest palindromic subsequence is "bbbb".

Example 2:
	Input:

	"cbbd"
	Output:
	2
One possible longest palindromic subsequence is "bb".
'''


'''
Solution outline: (DP)
	1. Every letter is a palindrome in itself (of length 1)
	2. Build a DP table (N x N) with 1 letter palindromes filled in.
		T[i][i] = 1
	3. Fill all two-letter palindromes
		T[i][i+1] = 2 if a[i] == a[i+1]
	4. i, j: 0, n-1
	    if a[i] == a[j], then T[i][j] = T[i+1][j-1] + 2 (because a[i], a[j] already form a two-letter palindrome)
		otherwise,
		a[i] =/= a[j], try a[i+1 .. j] and a[i .. j-1], and get the longest subsequence of the two - max (T[i+1][j], T[i][j-1])


Sample run 1:
============
bbbab

T = [][]

k = 1
  T = 
     [  b b b a b
	  b 1 0 0 0 0
	  b 0 1 0 0 0
	  b 0 0 1 0 0
	  a 0 0 0 1 0
	  b 0 0 0 1
	  ]
  T[0][0] = T[1][1] = T[2][2] = T[3][3] = T[4][4] = 1

k = 2
  "bb", "bb", "ba", "ab"
  (0,1) and (1,2) ==> palindromes: set them to 2
  T[0][1] = T[1][2] = 2
  T = 
     [  b b b a b
	  b 1 2 0 0 0
	  b 0 1 2 0 0
	  b 0 0 1 0 0
	  a 0 0 0 1 0
	  b 0 0 0 0 1
	  ]

0, n-1
  a[0] == a[4] == b
  T[0][[4] = 2 + T[1][3]
    => T[1][3]:
	     a[1] = b =/= a[3] = a
		 T[1][3] = max(T[1][2], T[2][3])
		   => T[1][2] = 2
		   => T[2][3]:
		         a[2] =/= a[3]
				 T[2][3] = max(T[3][3], T[2][2]) == 1
  T = 
     [  b b b a b
	  b 1 2 0 0 0
	  b 0 1 2 0 0
	  b 0 0 1 1 0
	  a 0 0 0 1 0
	  b 0 0 0 0 1
	  ]
		 T[1][3] = max(T[1][2], T[2][3]) == max(2, 1) == 2
  T = 
     [  b b b a b
	  b 1 2 0 0 0
	  b 0 1 2 2 0
	  b 0 0 1 1 0
	  a 0 0 0 1 0
	  b 0 0 0 0 1
	  ]
  T[0][[4] = 2 + T[1][3] == 2+2 = 4
  T = 
     [  b b b a b
	  b 1 2 0 0 4
	  b 0 1 2 2 0
	  b 0 0 1 1 0
	  a 0 0 0 1 0
	  b 0 0 0 0 1
	  ]
'''

def longest_palindromic_subsequence_len(s):
	if not s:
		return 0

	n = len(s)
	T = [[(1 if x==y else 0) for x in xrange(n)] for y in xrange(n)]

	# Store all 2-letter palindromes in the table
	for i in xrange(n-1):
		if s[i] == s[i+1]:
			T[i][i+1] = 2
		else:
			T[i][i+1] = 1

	# All 3-letter combinations, 4-, etc
	for k in xrange(3, n+1):
		for i in xrange(0, n-k+1):
			j = i+k-1
			if s[i] == s[j]:
				T[i][j] = T[i+1][j-1] + 2
			else:
				T[i][j] = max(T[i][j-1], T[i+1][j])

	return T[0][n-1]


if __name__ == '__main__':
	assert longest_palindromic_subsequence_len("bbbab") == 4
	assert longest_palindromic_subsequence_len("cbbd") == 2
	assert longest_palindromic_subsequence_len("underqualified") == 7 #deified
	assert longest_palindromic_subsequence_len("turboventilator") == 7 # rotator
	assert longest_palindromic_subsequence_len("r1a2c3e4c5a6r7") == 7 # racecar

