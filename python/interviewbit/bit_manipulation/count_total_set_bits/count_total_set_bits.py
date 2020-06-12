'''
https://www.interviewbit.com/courses/programming/topics/bit-manipulation/problems/count-total-set-bits/

Count Total Set Bits

Problem Description
Given a positive integer A, the task is to count the total number of set bits in the binary representation of all the numbers from 1 to A.
Return the count modulo 10^9 + 7.


Problem Constraints
1 <= A <= 10^9

Input Format
First and only argument is an integer A.

Output Format
Return an integer denoting the ( Total number of set bits in the binary representation of all the numbers from 1 to A )modulo 10^9 + 7.

Example Input
Input 1:
 A = 3

Input 2:
 A = 1

Example Output
Output 1:
 4
Output 2:
 1

Example Explanation
Explanation 1:
 DECIMAL    BINARY  SET BIT COUNT
    1          01        1
    2          10        1
    3          11        2
 1 + 1 + 2 = 4 
 Answer = 4 % 1000000007 = 4

Explanation 2:
 A = 1
  DECIMAL    BINARY  SET BIT COUNT
    1          01        1
 Answer = 1 % 1000000007 = 1
'''

'''
Solution Outline: Brute-force, O(nlogn)
	Enumerate 1-n, counting the bits set in each 1<=i<=n
'''
class Solution:
	def count_total_set_bits(self, n):
		# count bits set in an integer, i
		def bits_set(i):
			count = 0
			while i > 0:
				count += 1
				i &= (i-1)
			return count

		mod = 1000000007
		total_bits_set = 0
		for i in xrange(1, n+1):
			total_bits_set = (total_bits_set + bits_set(i)) % mod
		
		return total_bits_set


if __name__ == '__main__':
	s = Solution()
	assert s.count_total_set_bits(3) == 4
	assert s.count_total_set_bits(4) == 5
	assert s.count_total_set_bits(6) == 9
	assert s.count_total_set_bits(7) == 12
	assert s.count_total_set_bits(8) == 13



