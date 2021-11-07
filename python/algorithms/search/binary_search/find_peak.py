#encoding: utf-8
'''
Find a peak element.

A peak element is an element that is strictly greater than its neighbors.
Given an integer array nums, find a peak element, and return its index.
If the array contains multiple peaks, return the index to any of the peaks.
You may imagine that nums[-1] = nums[n] = -∞.
'''


'''
Solution Outline:
	0. mid = (l+h)/2
	1. if a[mid], a[mid+1] is increasing, then there is definitely a peak to the right.
		  a[mid+1..h]
	2. else, a[mid], a[mid+1] is a non-increasing sequence,
		there is definitely a peak to the left. a[l..mid]
'''
def find_peak_element_index(lst):
	l, h = 0, len(lst)-1
	while l < h:
		mid = l + (h-l)/2
		if lst[mid+1] > lst[mid]:
			l = mid+1
		else:
			h = mid
	return l


def find_peak_element(lst):
	return lst[find_peak_element_index(lst)]


if __name__ == '__main__':
	assert find_peak_element( [1,2,3,1] ) == 3
	assert find_peak_element( [1,2,1,3,5,6,4] ) in [2,6]


