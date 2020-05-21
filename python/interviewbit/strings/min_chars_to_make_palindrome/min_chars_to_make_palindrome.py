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
	Using two-pointers to compare B(left) and B(right)
	and then when A(left) and C(right) don't match, we have to include C after B to get 'BCACB'. (this isnt allowed)

	Instead, the characters needed to make 'BACB' palindromic are 'BCA' => 'BCABACB'

	Brute-force solution:
	  1. Start with adding 1 character from the right to the left, and check if its a palindrome
	  2. If it is, we are done, else add more characters from the right to the left
	  3. Slight optimization would be to skip the number of characters we just added from comparison.
	      for e.g., BC ... CB, we just added BC, we can skip comparing the first 2 and last 2 characters.

Sample run:
	s: "BACXB"

	Is 's' a palindrome? NO
	
	Add 'B' to the left
	s: "BBACXB" 
	   is s[1:-1]  == "BACX" a palindrome? NO

	Add 2 characters
	  s: "BXBACXB"
	    is s[2:-2] == "BAC" a palindrome? NO

	Add 3 characters
	   s: "BXCBACXB"
	     is s[3:-3] == "BA" a palindrome? NO

	Add 4 characters
	   s: "BXCABACXB"
	     is s[4:-4] == "B" a palindrome? YES
	return 4

Sample run 2:
	s: "AACECAAAA"

	is 's' a palindrome? NO

	Add 'A' to the left
	s: 'AAACECAAAA'
	 is s[1:-1] == "AACECAAA" a palindrome? NO

	Add 'AA' to the left
	s: 'AAAACECAAAA'
	  is s[2:-2] == 'AACECAA' a palindrome? YES

Alternately,
	Simulate adding 1 character to the left
	 => s: 'A' + "AACECAAAA"
	 we check if s[0:-1] is a palindrome
	 is "AACECAAA" a palindrome? NO

    Simulate adding 2 characters to the left
	  => s: "AA" + "AACECAAAA"
	  we check if s[0:-2] is a palindrome
      is "AACECAA" a palindrome? YES
	return 2
'''
class Solution:
	def min_chars_to_make_palindrome(self, A):
		# check if A[lb:ub] is a palindrome
		def is_palindrome(A, lb, ub):
			while lb < ub:
				if A[lb] != A[ub]:
					return False
				lb += 1
				ub -= 1

			return True

		n = len(A)

		# A is already a palindrome
		# no additions needed
		if not A or is_palindrome(A, 0, n-1):
			return 0

		j = 1
		while j < n-1:
			if is_palindrome(A, 0, n-j-1):
				return j
			j += 1

		return j


if __name__ == '__main__':
	s = Solution()
	assert s.min_chars_to_make_palindrome("AB") == 1
	assert s.min_chars_to_make_palindrome("racecar") == 0
	assert s.min_chars_to_make_palindrome("BACXB") == 4
	assert s.min_chars_to_make_palindrome("ABC") == 2
	assert s.min_chars_to_make_palindrome("AACECAAAA") == 2

