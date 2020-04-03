'''
https://www.interviewbit.com/problems/maximum-consecutive-gap/

Maximum Consecutive Gap

Given an unsorted array, find the maximum difference between the successive elements in its sorted form.

Try to solve it in linear time/space.

Example :
	Input : [1, 10, 5]
	Output : 5 

Return 0 if the array contains less than 2 elements.
You may assume that all the elements in the array are non-negative integers and fit in the 32-bit signed integer range.
You may also assume that the difference will not overflow.
'''

'''
Solution Outline:

	Create 'n' slots and distribute the array items into one of these slots.
	Min element occupies slot0, max element occupies slot (n-1)
	All other numbers use some slot in between.
	For each slot, store the min and max.
	  Max consecutive gap is max{
	                              gap within any slot,
								  gap between consecutive non-empty slots (min of current slot - max of previous slot)
								  }

2 6 12 35 67 72 88 90 95 100
0 1 2   3 4  5  6  7  8  9

when a[i] == min
index = (a[i]-min) * X == 0
when a[i] == max,
index = (a[i]-min) * X == (n-1)

X == (n-1)/(max-min)
=>
min: (min-min)*X = 0
max: (max-min)*(n-1)/(max-min) = n-1

for any a[i],
index = (a[i] - min) * (n-1)/(max-min)

a[i] = 2, index = 0
a[i] = 6, index = 0
a[i] = 12, index = 1
a[i] = 35, index = 3
a[i] = 67, index = 6
a[i] = 72, index = 7
a[i] = 88, index = 8
a[i] = 90, index = 8
a[i] = 95, index = 9
a[i] = 100, index = 9
'''
class Solution:
	def max_consecutive_gap(self, A):
		n = len(A)

		if n < 2:
			return 0

		slots = [None]*n
		minimum = maximum = A[0]
		for i in xrange(1, n):
			if A[i] < minimum:
				minimum = A[i]
			elif A[i] > maximum:
				maximum = A[i]

		# All elements in the list are identical
		# Maximum gap is 0
		# also, stop divide/by-zero while calculating index
		# by returning right here
		if maximum == minimum:
			return 0

		for x in A:
			index = ((x - minimum) * (n-1)) / (maximum-minimum)
			if slots[index] is None:
				slots[index] = [x, x]
			else:
				slots[index][0] = min(slots[index][0], x)
				slots[index][1] = max(slots[index][1], x)

		max_gap = slots[0][1] - slots[0][0] # slot 0 will never be empty
		i = 1
		prev_slot = slots[0][1]
		while i < n:
			if slots[i] is not None:
				gap_in_current_slot = slots[i][1] - slots[i][0]
				gap_curr_prev_slot = slots[i][0] - prev_slot
				prev_slot = slots[i][1]

				max_gap = max(max_gap, gap_in_current_slot, gap_curr_prev_slot)

			i += 1
			
		return max_gap



if __name__ == '__main__':
	s = Solution()
	assert s.max_consecutive_gap([1]) == 0
	assert s.max_consecutive_gap([100, 100, 100, 100]) == 0
	assert s.max_consecutive_gap([1, 10, 5]) == 5
	assert s.max_consecutive_gap([2, 6, 12, 35, 67, 72, 88, 90, 95, 100]) == 32
	assert s.max_consecutive_gap([2, 11, 20, 29, 37, 46, 55, 64, 73, 82, 90, 100]) == 10

