'''
Binary search on a sorted list
'''


# A Recursive binary search implementation
# Return the index where the item 'x' was found on the list
def binary_search_r(lst, x):
	# Find x in lst[l:h]
	def binary_search_helper(lst, x, l, h):
		# List's low and high indices have criss-crossed
		# => x could not be found
		if l > h:
			return -1

		mid = (l+h)/2
		if lst[mid] == x:
			return mid
		elif x > lst[mid]:
			# search in second half of the sub-array
			return binary_search_helper(lst, x, mid+1, h)
		else:
			# search in first half of the sub-array
			return binary_search_helper(lst, x, l, mid-1)


	# Call the helper
	return binary_search_helper(lst, x, 0, len(lst)-1)



# Iterative implementation of the binary search
def binary_search_i(lst, x):
	l, h = 0, len(lst)-1
	while l <= h:
		mid = (l+h)/2
		if lst[mid] == x:
			return mid
		elif x > lst[mid]:
			# search in second half of the sub-array
			l = mid+1
		else:
			# search in first half of the sub-array
			h = mid-1

	# Couldn't find 'x'
	return -1



# If not explicity called, use recursive binary search as default
binary_search = binary_search_r


def test_binary_search(search_f):
	assert(search_f([1,2,3,4,5], 2) == 1)
	assert(search_f([1,2,3,4,5], 1) == 0)
	assert(search_f([1,2,3,4,5], 5) == 4)
	assert(search_f([1,2,3,4,5], 0) == -1)

	assert(search_f((1,2,3,4), 5) == -1)
	assert(search_f((1,2,3,4), 0) == -1)
	assert(search_f((1,2,3,4), 1) == 0)
	assert(search_f((1,2,3,4), 2) == 1)
	assert(search_f((1,2,3,4), 3) == 2)
	assert(search_f((1,2,3,4), 4) == 3)

	assert(search_f([2,4,6, 8], 2) == 0)
	assert(search_f([2,4,6, 8], 4) == 1)
	assert(search_f([2,4,6, 8], 6) == 2)
	assert(search_f([2,4,6, 8], 8) == 3)
	assert(search_f([2,4,6, 8], 1) == -1)
	assert(search_f([2,4,6, 8], 3) == -1)
	assert(search_f([2,4,6, 8], 5) == -1)
	assert(search_f([2,4,6, 8], 7) == -1)




# basic testcases
if __name__ == "__main__":
	test_binary_search(binary_search)
	test_binary_search(binary_search_i)

