'''
https://www.interviewbit.com/problems/binary-representation/

Binary Representation
Given a number N >= 0, find its representation in binary.

Example:
	if N = 6,
	binary form = 110
'''

class Solution:
	# @param A : integer
	# @return a strings
	def findDigitsInBinary(self, A):
		if not A:
			return '0'

		binary = ""
		while A:
			binary = str(A & 1) + binary
			A /= 2

		return binary


if __name__ == '__main__':
	s = Solution()
	assert s.findDigitsInBinary(0) == '0'
	assert s.findDigitsInBinary(18) == '10010'
	assert s.findDigitsInBinary(10) == '1010'
	assert s.findDigitsInBinary(15) == '1111'
