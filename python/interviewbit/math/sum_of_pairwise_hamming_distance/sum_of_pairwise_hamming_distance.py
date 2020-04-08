#encoding: utf-8
'''
https://www.interviewbit.com/problems/power-of-two-integers/

Sum of pairwise Hamming Distance

Hamming distance between two non-negative integers is defined as the number of positions at which the corresponding bits are different.

For example,
HammingDistance(2, 7) = 2, as only the first and the third bit differs in the binary representation of 2 (010) and 7 (111).

Given an array of N non-negative integers, find the sum of hamming distances of all pairs of integers in the array.
Return the answer modulo 1000000007.

Example
Let f(x, y) be the hamming distance defined above.
A=[2, 4, 6]
We return,
f(2, 2) + f(2, 4) + f(2, 6) + 
f(4, 2) + f(4, 4) + f(4, 6) +
f(6, 2) + f(6, 4) + f(6, 6) = 

0 + 2 + 1
2 + 0 + 1
1 + 1 + 0 = 8
'''

'''
Solution Outline: (Brute force: O(n²))
	Compute pair-wise hamming distances between all unique pairs.
	hd(x,y) = hd(y,x), so calculating hd(x,y)*2, will account for both hd(x,y) and hd(y.x)
	hd(x,x) = 0

	To calculate hamming distances, hd(x,y)
	  compute xor(x, y) and count the bits set in the result.

	x = 010
	y = 100
   ---------
  x^y = 110
   ---------
   hd(2, 4) == 2
'''
class Solution:
	# return the number of bits set in n in O(b) time
	# b: number of bits actually set in n (b <= log₂(n): the number of bits in binary(n))
	def bits_set(self, n):
		count = 0
		while n:
			count += 1
			n &= (n-1)

		return count

	# Find hamming distance of all pairs of numbers above the
	# primary digonal, and add them up.
	# i,e, hd(A[x], A[y]): (x < y) {0<=x<n, x<y<n}
	def pairwise_hamming_distance(self, A):
		pairwise_hd = 0
		for i in xrange(len(A)):
			for j in xrange(i+1, len(A)):
				n = A[i] ^ A[j]
				pairwise_hd += (self.bits_set(n) << 1)

		return pairwise_hd


if __name__ == '__main__':
	s = Solution()
	assert s.pairwise_hamming_distance([2,4,6]) == 8




