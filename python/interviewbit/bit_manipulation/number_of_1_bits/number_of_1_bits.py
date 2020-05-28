'''
https://www.interviewbit.com/problems/number-of-1-bits/

Number of 1 Bits

Write a function that takes an unsigned integer and returns the number of 1 bits it has.

Example:
	The 32-bit integer 11 has binary representation

	00000000000000000000000000001011
	so the function should return 3.

Note that since Java does not have unsigned int, use long for Java
'''

'''
Solution Outline:
	n & (n-1) reduces the count of bits set by 1 by resetting the rightmost bit set.
	Keep moving by n = n & (n-1) and count as long as n is not 0

	Sample run:
		n = 11 (1011)

		n & (n-1)
		1011
		1010
		----
		1010 = decimal(10)

		n = 10 (1010)
		n & (n-1)
		1010
		1001
		----
		1000 = 8

		n = 8 (1000)
		n & (n-1) == 
	    1000
		0111
		----
		0000

		=> 3 bits set in 11
'''

class Solution:
	# @param n : integer
	# @return an integer
	def numSetBits(self, n):
		count = 0
		while n:
			count += 1
			n &= (n-1)

		return count


if __name__ == '__main__':
	s = Solution()
	assert s.numSetBits(11) == 3
	assert s.numSetBits(18) == 2
	assert s.numSetBits(15) == 4


