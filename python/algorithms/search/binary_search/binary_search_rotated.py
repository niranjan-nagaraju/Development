'''
Binary search in a rotated array
'''


from find_pivot import find_pivot

'''
Solution #1:
	1. Find a pivot in O(logn) time
	2. Then do binary search that accounts for indices that wrap around in a circular manner.
'''
def binary_search_rotated(lst, key):
	pivot = find_pivot(lst)

	n = len(lst)

	l, h = 0, n-1
	while l <= h:
		mid_ = (l+h)/2

		# adjust mid to shift by 'pivot' units
		mid = (mid_ + pivot) % n

		if lst[mid] == key:
			return mid
		elif key > lst[mid]:
			# key is in the right half
			l = (mid_ + 1)
		else: # key < lst[mid]
			# key is in the left half
			h = (mid_ - 1)

	# couldn't find key
	return -1



if __name__ == '__main__':
	assert find_pivot([4,5,6,7,1,2]) == 4
	assert binary_search_rotated([4,5,6,7,1,2], 1) == 4
	assert binary_search_rotated([4,5,6,7,1,2], 2) == 5
	assert binary_search_rotated([4,5,6,7,1,2], 0) == -1
	assert binary_search_rotated([4,5,6,7,1,2], 4) == 0
	assert binary_search_rotated([4,5,6,7,1,2], 5) == 1
	assert binary_search_rotated([4,5,6,7,1,2], 6) == 2
	assert binary_search_rotated([4,5,6,7,1,2], 7) == 3

	assert find_pivot([4,5,1,2,3]) == 2
	assert binary_search_rotated([4,5,1,2,3], 4) == 0
	assert binary_search_rotated([4,5,1,2,3], 5) == 1
	assert binary_search_rotated([4,5,1,2,3], 1) == 2
	assert binary_search_rotated([4,5,1,2,3], 2) == 3
	assert binary_search_rotated([4,5,1,2,3], 3) == 4
	assert binary_search_rotated([4,5,1,2,3], 6) == -1
	assert binary_search_rotated([4,5,1,2,3], 0) == -1

	assert find_pivot([3,1,3]) == 1
	assert binary_search_rotated([3,1,3], 0) == -1
	assert binary_search_rotated([3,1,3], 3) == 2
	assert binary_search_rotated([3,1,3], 1) == 1

	assert find_pivot([2,2,2,0,1]) == 3
	assert binary_search_rotated([2,2,2,0,1], 2) == 0 # either of 0,1,2 will do
	assert binary_search_rotated([2,2,2,0,1], 0) == 3
	assert binary_search_rotated([2,2,2,0,1], 1) == 4
	assert binary_search_rotated([2,2,2,0,1], -1) == -1
	assert binary_search_rotated([2,2,2,0,1], 3) == -1

	# Traditional binary search on an unrotated sorted array
	# should work just as well
	assert find_pivot([1,2,3,4,5,6,7]) == 0
	assert binary_search_rotated([1,2,3,4,5,6,7], 1) == 0
	assert binary_search_rotated([1,2,3,4,5,6,7], 2) == 1
	assert binary_search_rotated([1,2,3,4,5,6,7], 3) == 2
	assert binary_search_rotated([1,2,3,4,5,6,7], 4) == 3
	assert binary_search_rotated([1,2,3,4,5,6,7], 5) == 4
	assert binary_search_rotated([1,2,3,4,5,6,7], 6) == 5
	assert binary_search_rotated([1,2,3,4,5,6,7], 7) == 6

