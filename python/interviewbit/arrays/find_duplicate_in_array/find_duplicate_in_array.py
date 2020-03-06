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

class Solution:
	def find_duplicate(self, A):
		l = set()
		for x in A:
			if x in l:
				return x
			l.add(x)

		return -1


if __name__ == '__main__':
	s = Solution()
	assert s.find_duplicate([3,4,1,4,1]) == 4
	assert s.find_duplicate([3,4,2,5,1]) == -1
