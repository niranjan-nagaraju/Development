#encoding: utf-8
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
Solution Outline:
	A number of the form 2ˣ-1 (i.e x 1s) will have (x * 2ˣ⁻¹) bits set between 1 to 2ˣ-1.
	e.g,
	  n = 3, x = 1
	  00
	  01
	  10
	  11
	  -> total number of bits set = 4
	  = 2*2¹

	  n = 7
	  000
	  001
	  010
	  011
	  100
	  101
	  110
	  111
	  -> total number of set bits = 12
	  = 3*2²

	For any other 'n' that is not 2ˣ-1,
	   1. Find 'm' the leftmost set-bit
	      Count set bits till 2ᵐ-1 => (m*2ᵐ⁻¹)
		  Count 1 for each number from 2ᵐ to n == +n-(2ᵐ-1)
	   2. Repeat for the next leftmost set-bit until all set bits are processed
	
	E.g,
	  n = 13 (0b1101)
	  bits: 3 2 1 0
	        1 1 0 1
	  leftmost-bit set = 3
	  Divide the numbers between 1 - 13 into two sets
	    set 1: 1 to 2³-1 = 1 to 7
		set 2: 8 to 13
	  0 0 0 0
	  0 0 0 1
	  0 0 1 0
	  0 0 1 1
	  0 1 0 0
	  0 1 0 1
	  0 1 1 0
	  0 1 1 1
	  -------
	  1 0 0 0
	  1 0 0 1
	  1 0 1 0
	  1 0 1 1
	  1 1 0 0
	  1 1 0 1
	    set 1: total set bits = 3 * 2³⁻¹ = 3*4 = 12
		set 2: Count 6 bits for each element (13-7) in set 2 + recursively computed bits set for the next 3 bits (000-101)
		total_bits_set(13) = 12 + 6 + total_bits_set(5)
		                   = 18 + total_bits_set(5)

		total_set_bits(5)
		   left-most set bit is 2
		   Divide the numbers between 1 - 5 into two sets
		     set 1: 1 to 2²-1 = 1 to 3
			 set 2: 4 to 5
		 0 0 0
		 0 0 1
		 0 1 0
		 0 1 1
		 -----
		 1 0 0
		 1 0 1
		set 1: total set bits = 2 * 2²⁻¹ = 2*2 = 4
		set 2: Count 2 bits for each element (5-3) in set 2 + recursively computed bits set for the next 2 bits (00-01)
		total_bits_set(5) = 4 + 2 + total_bits_set(1)
		                   = 6 + total_bits_set(1)

		total_bits_set(1) = 1
		   left-most set bit is 1
		   Divide the numbers between 1 - 1 into two sets
		     set 1: 1
			 set 2: {}
			 0
			 1
			 -
			 {}
			 set 1: total set bits = 1 * 2¹⁻¹ = 1*1 = 1
			 set 2: Count 0 bits for each element in set 2, (1-1)
			   = 1
		   
		=>
		total_bits_set(5) = 6 + total_bits_set(1)
						   = 6 + 1
						   = 7
		=>
		total_bits_set(13) = 18 + total_bits_set(5)
						   = 18 + 7
						   = 25
'''
class Solution:
	def count_total_set_bits(self, n):
		mod = 1000000007
		import math

		# Total (bit)width of n
		m = int(math.log(n, 2))

		total_set_bits = 0
		while n:
			if n == 1:
				total_set_bits += 1
				break

			if n & (1<<m):
				# Add (m*2ᵐ⁻¹)+n-(2ᵐ-1)
				total_set_bits = (total_set_bits + m * (1<<(m-1)) + n-(1<<m)+1) % mod
				# Clear mᵗʰ bit in n
				n -= (1<<m)
			m -= 1

		return total_set_bits 


if __name__ == '__main__':
	s = Solution()
	assert s.count_total_set_bits(13) == 25
	assert s.count_total_set_bits(3) == 4
	assert s.count_total_set_bits(4) == 5
	assert s.count_total_set_bits(6) == 9
	assert s.count_total_set_bits(7) == 12
	assert s.count_total_set_bits(8) == 13



