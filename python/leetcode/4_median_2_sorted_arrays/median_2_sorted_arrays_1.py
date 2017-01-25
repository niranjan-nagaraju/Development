'''
https://leetcode.com/problems/median-of-two-sorted-arrays/

There are two sorted arrays nums1 and nums2 of size m and n respectively.

Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).

Example 1:

nums1 = [1, 3]
nums2 = [2]

The median is 2.0

Example 2:

nums1 = [1, 2]
nums2 = [3, 4]

The median is (2 + 3)/2 = 2.5
'''


# Solution #1: 
#	O(m+n) space and time complexity
#   1. Merge both sorted arrays a[] and b[] into a single sorted array c[]
#   2. Calculate median on the sorted array c[]

class Solution(object):
	# Complexity: O(m+n): space, O(m+n): time
	def findMedianSortedArrays(self, nums1, nums2):
		"""
		:type nums1: List[int]
		:type nums2: List[int]
		:rtype: float
		"""
		c = merge(nums1, nums2)
		return median(c)
		


# Merge two sorted arrays 'a' and 'b' into a single sorted array
def merge(a, b):
	c = [0] * (len(a) + len(b))
	i, j, k = 0, 0, 0

	while i < len(a) and j < len(b):
		if (a[i] <= b[j]):
			c[k] = a[i]
			i += 1
		else:
			c[k] += b[j]
			j += 1

		k += 1

	while i < len(a):
		c[k] = a[i]
		i += 1
		k += 1


	while j < len(b):
		c[k] = b[j]
		j += 1
		k += 1


	return c


# median is a[n/2] if n:odd, avg of middle 2 elements if n:even
def median(a):
	n = len(a)
	# (n-1)/2 and n/2 are the same for odd 'n', 
	# e.g., n: 7, (n-1)/2 == 3, n/2 == 3
	return (a[(n-1)/2] + a[n/2])/2.0


if __name__ == "__main__":
	# Verify median() works alright
	assert(median([1,3,3,6,7,8,9]) == 6.0)
	assert(median([1,2,3,4,5,6,8,9]) == 4.5)

	# Testcases for median of 2 sorted arrays
	s = Solution()
	assert(s.findMedianSortedArrays([1,3], [2,4]) == 2.5)
	assert(s.findMedianSortedArrays([1,3], [2]) == 2)
	assert(s.findMedianSortedArrays([2], [1,3]) == 2)
	assert(s.findMedianSortedArrays([2,4,10], [1,3,5,6,7,8,9]) == 5.5)
	assert(s.findMedianSortedArrays([2,4,6,8,10], [1,3,5,7,9]) == 5.5)


