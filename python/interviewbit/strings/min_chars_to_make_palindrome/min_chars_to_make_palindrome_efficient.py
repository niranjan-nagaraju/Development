'''
https://www.interviewbit.com/problems/minimum-characters-required-to-make-a-string-palindromic/

Minimum Characters required to make a String Palindromic

Given an string A. The only operation allowed is to insert characters in the beginning of the string.

Find how many minimum characters are needed to be inserted to make the string a palindrome string.


Input Format
The only argument given is string A.

Output Format
Return the minimum characters that are needed to be inserted to make the string a palindrome string.

For Example
Input 1:
    A = "ABC"
Output 1:
    2
Explanation 1:
	Insert 'B' at beginning, string becomes: "BABC".
	Insert 'C' at beginning, string becomes: "CBABC".

Input 2:
    A = "AACECAAAA"
Output 2:
    2
Explanation 2:
	Insert 'A' at beginning, string becomes: "AAACECAAAA".
	Insert 'A' at beginning, string becomes: "AAAACECAAAA".
'''

'''
Solution Outline:
	Consider 'BACB'
	Add the reverse string to the end with a delimiter, say '$' separating the two.
	'BACB$BCAB'
	Compute a LPS array (Longest-prefix which is also a suffix from knuth-morris-pratt algorithm)
    s: B A C B $ B C A B
       0 1 2 3 4 5 6 7 8
 LPS: [0 0 0 1 0 1 0 0 1]
   LPS[-1] = 1
   "BACB" -> length: 4
   Minimum number of characters needed to make "BACB" a palindrome is 4-1 = 3

	Infact, the characters needed to make 'BACB' palindromic are 'BCA' => 'BCABACB'

Sample run:
	s: "BACXB"
	s':   B A C X B $ B X C A B
	LPS: [0 0 0 0 1 0 1 0 0 0 1]
	min chars needed: 5-1 = 4


Sample run 2:
	s: "AACECAAAA"
	s':   A A C E C A A A A $ A A A A C E C A A
	LPS: [0 1 0 0 0 1 2 2 2 0 1 2 2 2 3 4 5 6 7
	len(s): 9
	LPS[-1] = 7
	Min chars needed = 9-7 = 2
'''
class Solution:
	def min_chars_to_make_palindrome(self, A):
		def compute_lps_array(A, n):
			lp = 0 # longest prefix-suffix so far
			lps = [0]*n

			i = 1 # LPS[0] is 0
			while i < n:
				if A[i] == A[lp]:
					lp += 1
					lps[i] = lp
					i += 1
				else:
					if lp == 0:
						lps[i] = 0
						i += 1
					else:
						# lp != 0
						# Backtrack by lps[lp-1] to see if we can
						# match past prefixes
						lp = lps[lp-1]
			return lps

		if not A:
			return 0

		n = len(A)
		lps = compute_lps_array(A+'$'+A[::-1], 2*n+1)
		return n-lps[-1]




if __name__ == '__main__':
	s = Solution()
	assert s.min_chars_to_make_palindrome("AB") == 1
	assert s.min_chars_to_make_palindrome("racecar") == 0
	assert s.min_chars_to_make_palindrome("BACB") == 3
	assert s.min_chars_to_make_palindrome("BACXB") == 4
	assert s.min_chars_to_make_palindrome("ABC") == 2
	assert s.min_chars_to_make_palindrome("AACECAAAA") == 2

