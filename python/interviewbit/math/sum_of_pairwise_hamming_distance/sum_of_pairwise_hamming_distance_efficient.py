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
Solution Outline: (Brute force: O(n))
	Let B be the width(number of bits) needed to store the maximum number in A
	For each bit, i, 0 <= i < B
		Find how many in A has their ith bit set, Let this be s, then (n-s) numbers in A have their ith bit unset.
		For the ith bit, the pair-wise hamming distance would be s*(n-s)*2 (as each (1,0) permutation contributes a 1)
		Sum all the bit-wise hamming distances.


Sample run:
	A: [2,4,6]

	B = 3
	pairwise_hd_sum = 0

	A[0]: 010
	A[1]: 100
	A[2]: 110

	Bit 0
	s = 2, (n-s) = 1
	pairwise_hd_sum = 2*1*2 = 4

	Bit 1
	s = 2, (n-s) = 1
	pairwise_hd_sum += 2*1*2 = 8

	Bit 2
	s = 0
	pairwise_hd_sum = 8
'''
import math
class Solution:
	# Find hamming distance of all pairs of numbers
	def pairwise_hamming_distance(self, A):
		pairwise_hd_sum = 0
		maximum = max(A)

		if maximum == 0:
			# All numbers in A are 0s
			return 0

		n = len(A)
		d = int((math.log(maximum, 2)))+1

		for i in xrange(d):
			count_set = 0
			for x in A:
				if x & (1<<i) != 0:
					count_set += 1

			pairwise_hd_sum += ((count_set) * (n-count_set)) << 1

		return pairwise_hd_sum % 1000000007


if __name__ == '__main__':
	s = Solution()
	assert s.pairwise_hamming_distance([2,4,6]) == 8
	assert s.pairwise_hamming_distance([1,2,3,4]) == 22
	assert s.pairwise_hamming_distance([1,2,3,4,5]) == 36
	assert s.pairwise_hamming_distance([1,100,200,300,400]) == 88
	assert s.pairwise_hamming_distance([0,0,0]) == 0
	assert s.pairwise_hamming_distance([0,1,2]) == 8



