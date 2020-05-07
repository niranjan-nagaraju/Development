'''
https://www.interviewbit.com/problems/longest-palindromic-substring/

Longest Palindromic Substring

Given a string S, find the longest palindromic substring in S.

Substring of string S:

S[i...j] where 0 <= i <= j < len(S)

Palindrome string:

A string which reads the same backwards. More formally, S is palindrome if reverse(S) = S.

Incase of conflict, return the substring which occurs first ( with the least starting index ).

Example :

Input : "aaaabaaa"
Output : "aaabaaa"
'''

'''
Solution Outline:
	1. Use DP - Start with finding palindromic substrings of length 1 and 2
	   DP[i][i] = T
	   DP[i][i+1] = T if A[i] == A[i+1]
	
	2. Then compute DP[i][j] as True if A[i:j] is a palindrome
	     which can be checked by A[i] == A[j] and DP[i-1:j-1] being True

	3. Start with i=0,j=2 {and compute all 3-letter palindromes}
        i=0,j=2, i=1,j=3, i=2,j=4, ...
        Then move onto 4-letters, all the way till n-letters
		Capture start_idx each time a higher length palindrome is found, and its length
		Return last updated A[start_idx:length] as the longest palindromic substring

e.g.,

  A: "xyraryz"

  DP table: {1-letter palindromes}
      x   y   r   a   r   y   z  
|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---+---+---+---+---+---+---+---|
| 0 | T |   |   |   |   |   |   |
| 1 |   | T |   |   |   |   |   |
| 2 |   |   | T |   |   |   |   |
| 3 |   |   |   | T |   |   |   |
| 4 |   |   |   |   | T |   |   |
| 5 |   |   |   |   |   | T |   |
| 6 |   |   |   |   |   |   | T |

  DP table: {2-letter palindromes}
      x   y   r   a   r   y   z  
|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---+---+---+---+---+---+---+---|
| 0 | T | F |   |   |   |   |   |
| 1 |   | T | F |   |   |   |   |
| 2 |   |   | T | F |   |   |   |
| 3 |   |   |   | T | F |   |   |
| 4 |   |   |   |   | T | F |   |
| 5 |   |   |   |   |   | T | F |
| 6 |   |   |   |   |   |   | T |


  DP table: {3-letter palindromes}
      x   y   r   a   r   y   z  
|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---+---+---+---+---+---+---+---|
| 0 | T | F | F |   |   |   |   |
| 1 |   | T | F | F |   |   |   |
| 2 |   |   | T | F | T |   |   |
| 3 |   |   |   | T | F | F |   |
| 4 |   |   |   |   | T | F | F |
| 5 |   |   |   |   |   | T | F |
| 6 |   |   |   |   |   |   | T |
s_idx = 2, max_len=3 => lps so far == A[2:2+3] = "rar"

  DP table: {4-letter palindromes}
      x   y   r   a   r   y   z  
|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---+---+---+---+---+---+---+---|
| 0 | T | F | F | F |   |   |   |
| 1 |   | T | F | F | F |   |   |
| 2 |   |   | T | F | T | F |   |
| 3 |   |   |   | T | F | F | F |
| 4 |   |   |   |   | T | F | F |
| 5 |   |   |   |   |   | T | F |
| 6 |   |   |   |   |   |   | T |

  DP table: {5-letter palindromes}
      x   y   r   a   r   y   z  
|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---+---+---+---+---+---+---+---|
| 0 | T | F | F | F | F |   |   |
| 1 |   | T | F | F | F | T |   |
| 2 |   |   | T | F | T | F | F |
| 3 |   |   |   | T | F | F | F |
| 4 |   |   |   |   | T | F | F |
| 5 |   |   |   |   |   | T | F |
| 6 |   |   |   |   |   |   | T |
s_idx = 1, max_len=4 => lps so far == A[1:1+4] = "yrary"

  DP table: {6-letter palindromes}
      x   y   r   a   r   y   z  
|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---+---+---+---+---+---+---+---|
| 0 | T | F | F | F | F | F |   |
| 1 |   | T | F | F | F | T | F |
| 2 |   |   | T | F | T | F | F |
| 3 |   |   |   | T | F | F | F |
| 4 |   |   |   |   | T | F | F |
| 5 |   |   |   |   |   | T | F |
| 6 |   |   |   |   |   |   | T |

  DP table: {7-letter palindromes}
      x   y   r   a   r   y   z  
|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 |
|---+---+---+---+---+---+---+---|
| 0 | T | F | F | F | F | F | F |
| 1 |   | T | F | F | F | T | F |
| 2 |   |   | T | F | T | F | F |
| 3 |   |   |   | T | F | F | F |
| 4 |   |   |   |   | T | F | F |
| 5 |   |   |   |   |   | T | F |
| 6 |   |   |   |   |   |   | T |

return "yrary"
'''

class Solution:
	def find_longest_palindromic_substring(self, A):
		n = len(A)

		# Create DP table of n x n
		# Mark DP[i][i] as palindromes
		DP = [[False]*n for _ in range(n)]

		for i in xrange(n):
			DP[i][i] = True

		s_Idx = 0
		max_len = 1

		# Find palindromes of length 2
		for i in xrange(n-1):
			if A[i] == A[i+1]:
				DP[i][i+1] = True
				if max_len != 2:
					# Update start idx if this is the first palindrome
					# we found for current length
					s_Idx = i
					max_len = 2

		# Find palindromes of lengths of 3 .. n
		for i in xrange(2, n):
			curr_len = i+1
			for j in xrange(n-i):
				if A[j] == A[j+i] and DP[j+1][j+i-1] == True:
					DP[j][j+i] = True
					# A[j:j+i] is a palindrome
					if max_len != curr_len:
						# Update start idx if this is the first palindrome
						# we found for current length
						s_Idx = j
						max_len = curr_len

		return A[s_Idx : s_Idx+max_len]



if __name__ == '__main__':
	s = Solution()
	assert s.find_longest_palindromic_substring("xyraryz") == 'yrary'
	assert s.find_longest_palindromic_substring("abcbd") == "bcb"
	assert s.find_longest_palindromic_substring("babad") == "bab"
	assert s.find_longest_palindromic_substring("abb") == "bb"
	assert s.find_longest_palindromic_substring("abbcc") == "bb"

