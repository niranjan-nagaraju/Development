'''
https://www.interviewbit.com/problems/search-in-bitonic-array/

Search in Bitonic Array!

Problem Description
Given a bitonic sequence A of N distinct elements, write a program to find a given element B in the bitonic sequence in O(logN) time.

NOTE:
A Bitonic Sequence is a sequence of numbers which is first strictly increasing then after a point strictly decreasing.

Problem Constraints
3 <= N <= 105
1 <= A[i], B <= 108
Given array always contain a bitonic point.
Array A always contain distinct elements.

Input Format
First argument is an integer array A denoting the bitonic sequence.
Second argument is an integer B.

Output Format
Return a single integer denoting the position (0 index based) of the element B in the array A if B doesn't exist in A return -1.

Example Input
Input 1:
 A = [3, 9, 10, 20, 17, 5, 1]
 B = 20

Input 2:
 A = [5, 6, 7, 8, 9, 10, 3, 2, 1]
 B = 30

Example Output
Output 1:
 3

Output 2:
 -1

Example Explanation
Explanation 1:
 B = 20 present in A at index 3

Explanation 2:
 B = 30 is not present in A
'''

'''
Solution Outline:
	1. A bitonic array has a single peak.
	2. Find the single peak using the peak-finder algorithm.
	   2.1 Split the array into two at the peak - The left side now contains only increasing elements, and the right only decreasing elements.
	3. Perform regular binary search on the left side of the peak, reverse binary search( assuming reverse-sorted array ) on the right.
	4. Return index returned by either of the binary searches.
'''


class Solution:
	def find_peak( self, lst ):
		l, h = 0, len(lst)-1
		while l < h:
			mid = l + (h-l)/2
			if lst[mid+1] > lst[mid]:
				# increasing sequence
				# look for peak on the right side.
				l = mid + 1
			else:
				# decreasing sequence
				# look for a peak on the left side
				h = mid
		return l

			
	def binary_search( self, lst, key, l, h, comparefn ):
		while l <= h:
			mid = l + (h-l)/2
			if comparefn(lst[mid], key) == 0:
				return mid
			if comparefn(lst[mid], key) == 1:
				# look to the left side
				h = mid-1
			else: # look to the right side
				l = mid+1
		return -1

	def bitonic_search( self, lst, key ):
		if not lst:
			return -1
		peak = self.find_peak( lst )
		match_left = self.binary_search(lst, key, 0, peak, cmp)
		reverse_cmp = lambda lhs, rhs: -cmp(lhs, rhs)
		match_right = self.binary_search(lst, key, peak+1, len(lst)-1, reverse_cmp)
		return match_left if match_left >=0 else match_right

if __name__ == '__main__':
	s = Solution()
	assert s.find_peak([1,2,3,1]) == 2
	assert s.find_peak([1,2,1,3,5,6,4]) in [1,5]
	assert s.bitonic_search([3, 9, 10, 20, 17, 5, 1], 20) == 3
	assert s.bitonic_search([5, 6, 7, 8, 9, 10, 3, 2, 1], 30) == -1

