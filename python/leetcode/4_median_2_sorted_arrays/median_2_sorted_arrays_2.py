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


# Solution #2: 
#	O(1) space and O(m+n) time complexity
#   1. Pretend to merge both sorted arrays a[] and b[] into an auxiliary array
#   2. Capture p = [n-1/2] and q = [n/2] smallest elements as we 'merge'
#   3. Average of p and q is the median


class Solution(object):
	# Complexity: O(m+n): space, O(m+n): time
	def __init__ (self):
		self.p = None
		self.q = None
		self.median = None

	
	def findMedianSortedArrays(self, nums1, nums2):
		"""
		:type nums1: List[int]
		:type nums2: List[int]
		:rtype: float
		"""
		return self.mergeAndfindMedian(nums1, nums2)
		

	# k: current idx of the (imaginary) consolidated sorted array 
	# while merging two sorted arrays
	# c: current element
	# n: length of the consolidated sorted array == sum(len(both arrays))
	# Once both p and q are set, calculate median.
	def calculate_median (self, k, c, n):
		if k == (n-1)/2:
			self.p = c
		if k == n/2:
			self.q = c
			self.median = (self.p + self.q)/2.0  # we found both p and q
			return True # we have calculated the median and it's ready

		# We don't yet have the median
		return False


	# Pretend to merge two sorted arrays 'a' and 'b' into a single sorted array
	# Get 'p' and 'q' - the middle two elements (if n is odd, p == q)
	# Calculate median == (p+q) / 2
	def mergeAndfindMedian(self, a, b):
		# If both a[] and b[] were merged into an auxiliary array
		# it'd be of length 'n'
		# then p == element at k == (n-1)/2 
		# and q == element at k == n/2

		n = (len(a) + len(b))
		i, j, k = 0, 0, 0

		while i < len(a) and j < len(b):
			if (a[i] <= b[j]):
				c = a[i]
				i += 1
			else:
				c = b[j]	
				j += 1

			if (self.calculate_median(k, c, n) == True):
				return self.median

			k += 1

		# Remainder of a[]
		while i < len(a):
			c = a[i]
			if (self.calculate_median(k, c, n) == True):
				return self.median

			i += 1
			k += 1


		# Remainder of b[]
		while j < len(b):
			c = b[j]
			if (self.calculate_median(k, c, n) == True):
				return self.median

			j += 1
			k += 1

		# Technically we'll never come till here
		return self.median 



if __name__ == "__main__":
	# Testcases for median of 2 sorted arrays
	s = Solution()
	assert(s.findMedianSortedArrays([1,3], [2,4]) == 2.5)
	assert(s.findMedianSortedArrays([1,3], [2]) == 2)
	assert(s.findMedianSortedArrays([2], [1,3]) == 2)
	assert(s.findMedianSortedArrays([2,4,10], [1,3,5,6,7,8,9]) == 5.5)
	assert(s.findMedianSortedArrays([2,4,6,8,10], [1,3,5,7,9]) == 5.5)


