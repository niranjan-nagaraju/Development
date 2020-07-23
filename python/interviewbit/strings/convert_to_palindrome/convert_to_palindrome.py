#encoding: utf-8
'''
https://www.interviewbit.com/problems/convert-to-palindrome/

Convert to Palindrome

Problem Description
Given a string A consisting only of lowercase characters, we need to check whether it is possible to make this string a palindrome after removing exactly one character from this.
If it is possible then return 1 else return 0.


Problem Constraints
3 <= |A| <= 105
A[i] is always a lowercase character.

Input Format
First and only argument is an string A.

Output Format
Return 1 if it is possible to convert A to palindrome by removing exactly one character else return 0.


Example Input
Input 1:
 A = "abcba"
Input 2:
 A = "abecbea"


Example Output
Output 1:
 1
Output 2:
 0


Example Explanation
Explanation 1:
 We can remove character ‘c’ to make string palindrome
Explanation 2:
 It is not possible to make this string palindrome just by removing one character
'''

'''
Solution Outline: (Brute-force)
	1. If A is already a palindrome, removing the middle element will still render A palindromic.
		e.g, 
			Case I:  A is a palindrome of odd-length
				A: "racecar"
					removing "e" from A
					A': raccar -> still a palindrome
			Case II: A is a palindrome of even-length
				A: "redder"
					removing "d" from A
					A': reder -> still a palindrome
	2. for each element x in A,
		check if removing it results in a palindrome
'''
class Solution:
	# Check if A[0:skip_index-1, skip_index+1..n-1]
	# is a palindrome
	def is_palindrome(self, A, skip_index):
		i, j = 0, len(A)-1
		while i < j:
			if i == skip_index:
				i += 1
			elif j == skip_index:
				j -= 1
			else:
				if A[i] != A[j]:
					return False
				i += 1
				j -= 1

		return True


	# Check if removing a character, one at a time
	# will leave the string a palindrome
	def can_convert_to_palindrome(self, A):
		if not A:
			return 0

		# A is already a palindrome
		if self.is_palindrome(A, -1):
			return 1

		for i in xrange(len(A)):
			if self.is_palindrome(A, i):
				# removing 'i' renders A a palindrome
				return 1

		return 0


if __name__ == '__main__':
	s = Solution()
	assert s.is_palindrome("abceba", 2) == True
	assert s.is_palindrome("abceba", 3) == True
	assert s.is_palindrome("abceba", 0) == False
	assert s.is_palindrome("abceba", 5) == False
	assert s.is_palindrome("abceba", 100) == False # nothing to skip

	assert s.can_convert_to_palindrome("racecar") == 1
	assert s.can_convert_to_palindrome("redder") == 1
	assert s.can_convert_to_palindrome("abceba") == 1
	assert s.can_convert_to_palindrome("abcdeba") == 0
	assert s.can_convert_to_palindrome("abecbea") == 0
	assert s.can_convert_to_palindrome("abcbea") == 1
