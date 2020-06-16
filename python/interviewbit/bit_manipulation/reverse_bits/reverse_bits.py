'''
https://www.interviewbit.com/problems/reverse-bits/

Reverse Bits

Reverse the bits of an 32 bit unsigned integer A.

Input Format:
    First and only argument of input contains an integer A

Output Format:
    return a single unsigned integer denoting minimum xor value

Constraints:
	0 <= A < 2^32

Example Input 1:
    A = 0
Example Output 1:
    0
Explanation 1:
        00000000000000000000000000000000  
=>      00000000000000000000000000000000


Example Input 2:
    A = 3
Example Output 2:
    3221225472
Explanation 2:
          00000000000000000000000000000011 
=>        11000000000000000000000000000000
'''

class Solution:
	def reverse_bits(self, A):
		m = 1
		reversed_A = 0
		for i in xrange(32):
			# extract mth bit
			bit = 1 if (A & m) else 0
			m <<= 1

			# append mth bit to the end
			reversed_A = (reversed_A << 1) | bit

		return reversed_A

	
if __name__ == '__main__':
	s = Solution()
	assert s.reverse_bits(0) == 0
	assert s.reverse_bits(3) == 3221225472

