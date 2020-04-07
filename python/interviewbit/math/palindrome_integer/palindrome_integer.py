#encoding: utf-8
'''
https://www.interviewbit.com/problems/palindrome-integer/

Palindrome Integer

Determine whether an integer is a palindrome. Do this without extra space.

A palindrome integer is an integer x for which reverse(x) = x where reverse(x) is x with its digit reversed.
Negative numbers are not palindromic.

Example :
	Input : 12121
	Output : True

	Input : 123
	Output : False
'''

'''
Solution Outline:
	Use 2 pointers at either ends, and compare their digits until they meet in the middle
	If any of the two digits dont match, the number is not a palindrome

	Let d be the number of digits in the number. {n: nₔ₋₁, nₔ₋₂, ..., n₀}
	The left-most and right-most digits are obtained by
	  (n/10ᵈ⁻¹, n % 10) respectively.

	For e.g, n : 12345
	d = 5
	leftmost digit: 12345 / 10⁴ == 1
	rightmost digit: 12345 % 10 == 5

	If the left-most and right-most digits match, we yank them out of the number to reduce the window
	12321 --> 232
	and compare the end digits again.

	To yank the leftmost anf rightmost digits of n
	  n = (n % 10ᵈ⁻¹) / 10
	For e,g, 
	 n = 12321
	 d = 5
	 n % 10⁴ == 2321
	 /10 == 232
'''

class Solution:
	def is_palindrome_integer(self, n):
		def count_digits(n):
			d = 0
			while n:
				d += 1
				n /= 10
			return d


		if n < 0:
			return 0

		d = count_digits(n)
		power10 = 10 ** (d-1)
		while power10 > 1:
			l = n / power10
			r = n % 10

			if l != r:
				return 0

			# Strip leftmost and rightmost numbers
			n = (n % (power10)) / 10
			power10 /= 100

		return 1


if __name__ == '__main__':
	s = Solution()
	assert s.is_palindrome_integer(-123321) == 0
	assert s.is_palindrome_integer(123321) == 1
	assert s.is_palindrome_integer(12321) == 1
	assert s.is_palindrome_integer(1) == 1
	assert s.is_palindrome_integer(11) == 1
	assert s.is_palindrome_integer(12341) == 0

