#encoding: utf-8
'''
https://www.interviewbit.com/problems/find-duplicate-in-array/

Find Duplicate in Array
Given a read only array of n + 1 integers between 1 and n, find one number that repeats in linear time using less than O(n) space and traversing the stream sequentially O(1) times.

Sample Input:
	[3 4 1 4 1]
Sample Output:
	1

If there are multiple possible answers ( like in the sample case above ), output any one.
If there is no duplicate, output -1
'''

'''
Solution Outline:
	Time: O(n), Memory: O(√n)
	0. Use a sqrt decomposition-like structure.
	  For n, m=√n, Use n/m buckets each bucket storing the combined frequency of 'm' numbers in its respective buckets.
	  e.g., n = 32, m = √n = 5
	     Number of buckets = ceil(n/m) = ceil(6.4) == 7
		 Bucket 0: holds [0,1,2,3,4]
		 Bucket 1: holds [5,6,7,8,9]
		 Bucket 2: holds [10,11,12,13,14]
		 Bucket 3: holds [15,16,17,18,19]
		 Bucket 4: holds [20,21,22,23,24]
		 Bucket 5: holds [25,26,27,28,29]
		 Bucket 6: holds [30,31]

	1. Scan the array once adding up cumulative frequencies of items belonging in a bucket.
	2. Scan the buckets themselves. Each bucket is expected to hold 'm' items if the numbers corresponding to the bucket do not repeat.
	   Let the first bucket that holds >m items be 'b'
	   Range of items in bucket 'b' would be (b-1)*m to b*m-1
	   Start a lookup table[m] mapping b*m to [0], and b*m+m-1 to [m-1]
	   Count frequencies again until any lookup-table[i] reaches > 1
	NOTE: Initialize last-bucket frequencies to [m - n%m] instead of 0, so we dont need to have a special check for the last bucket
	      when n is not divisble by m
		  e.g.,
		  n = 32, m = 5
		  bucket[6] can only hold 2 items
		  so initialize it with (5 - 32%5) == 3
'''
from math import ceil,sqrt
class Solution:
	def find_duplicate(self, A):
		n = len(A)
		m = int(sqrt(n))
		nBuckets = int(ceil(float(n)/m))
		buckets = [0]*nBuckets
		buckets[-1] = (m - n%m)

		for x in A:
			bkt_idx = (x-1) / m
			buckets[bkt_idx] += 1

		b = -1
		for b in xrange(nBuckets):
			if buckets[b] > m:
				break

		# None of the buckets contain > m elements
		# => no repitition in that bucket
		# unless there are missing numbers as well
		# e.g, m = 3, bucket[0]: {1,1,2}
		# bucket[0] contains 3 items but 1 is repeated
		# which wont be registered
		# However, considering there are (n+1) numbers with [1..n]
		# atleast one bucket is expected to have > m items (WHY?!!)
		if b == -1:
			return -1

		lookup_table = [0]*m
		l = b*m
		u = l+m-1
		for x in A:
			if (x-1) >=l and (x-1) <= u:
				# We have already seen another [x]
				if lookup_table[(x-1)-l] > 0:
					return x
				lookup_table[(x-1)-l] += 1
		
		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.find_duplicate([3,4,1,4,1]) == 4
	assert s.find_duplicate([3,4,2,5,1,5]) == 5
	assert s.find_duplicate([3,4,2,5,1]) == -1
	assert s.find_duplicate([10, 1, 2, 3, 5, 4, 9, 8, 5, 6, 4]) == 5

