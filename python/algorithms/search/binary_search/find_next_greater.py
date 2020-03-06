'''
Given a sorted array, A, and a 'key'
Return the index, i, of the next greater number in the array, a[i] > key
'''

'''
Solution Outline:
	Apply binary search to repeatedly divide the array restricting the half of the array a[mid] > key
	If a[mid] > key,
	   Find another mid to the left, a[0..mid-1] that is > key
	   If found, replace that as the next greater number,
	   Else, return previously found index.

Sample run:
	A: [1, 4, 5, 6, 8, 9, 10, 13], key : 7
        0  1  2  3  4  5   6  7

    l = 0, h = 7	
    mid = (0+7)/2 == 3
    a[mid] = a[3] = 6 < key => look to the right

    l = mid+1 == 4, h = 7
    mid = (4+7)/2 = 5
    a[mid] = a[5] = 9 > key, nge = 5 => look for a better match to the left

    l = 4, h = mid-1 = 4
    mid = (4+4)/2 = 4
    a[mid] = a[4] > key, nge = 4 => look for a better match to the left

    l = 4, h = mid-1 = 3 (l > h)
      return 4, a[4] = 8 is the number immediately greater than (key=6) in the array
'''


# Find and return the next greater number's index for a given key
# within lst[l..h]
# NOTE: lst is assumed to sorted (Non-decreasing order)
def find_next_greater(lst, key, l=0, h=None):
	if h == None:
		h = len(lst)-1
	nge_idx = -1
	while l <= h:
		mid = (l+h)/2
		if lst[mid] > key:
			nge_idx = mid
			# Found a candidate for NGE
			# Look to the left so if we can find a better match
			h = mid-1
		else:
			# lst[mid] <= key
			# Look to the right
			l = mid+1

	return nge_idx



# Find and return the next greater number's index for a given key
# within lst[l..h]
# NOTE: lst is assumed to reverse-sorted (Non-increasing order)
# NOTE: A comparison function can merge both these into a singe function
# FIXME
def find_next_greater_in_reverse_sorted(lst, key, l=0, h=None):
	if h == None:
		h = len(lst)-1
	nge_idx = -1
	while l <= h:
		mid = (l+h)/2
		if lst[mid] > key:
			nge_idx = mid
			# Found a candidate for NGE
			# Look to the right so if we can find a better match
			l = mid+1
		else:
			# lst[mid] <= key
			# Look to the left
			h = mid-1

	return nge_idx



if __name__ == '__main__':
	assert find_next_greater([1, 4, 5, 6, 8, 9, 10, 13], 7) == 4
	assert find_next_greater([1, 4, 5, 6, 7, 9, 10, 13], 7) == 5
	assert find_next_greater([1, 1, 1, 7], 7) == -1
	assert find_next_greater([1, 2, 3, 4, 5, 6, 7, 8], 7) == 7
	assert find_next_greater([1, 2, 3, 4, 5, 6, 7, 8], 7, 4, 7) == 7
	assert find_next_greater([1, 2, 3, 4, 5, 6, 7, 8], 7, 1, 6) == -1

	assert find_next_greater_in_reverse_sorted([13, 10, 9, 8, 6, 5, 4, 1], 7) == 3
	assert find_next_greater_in_reverse_sorted([13, 10, 9, 7, 6, 5, 4, 1], 7) == 2
	assert find_next_greater_in_reverse_sorted([7, 1, 1, 1], 7) == -1
	assert find_next_greater_in_reverse_sorted([8, 7, 6, 5, 4, 3, 2, 1], 7) == 0
	assert find_next_greater_in_reverse_sorted([8, 7, 6, 5, 4, 3, 2, 1], 7, 4, 7) == -1
	assert find_next_greater_in_reverse_sorted([8, 7, 6, 5, 4, 3, 2, 1], 7, 1, 6) == -1
	assert find_next_greater_in_reverse_sorted([8, 7, 6, 5, 4, 3, 2, 1], 7, 0, 5) == 0

