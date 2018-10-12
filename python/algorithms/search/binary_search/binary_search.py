'''
Binary search on a sorted list
'''


# A Recursive binary search implementation
def binary_search_r(lst, x):
	# Find x in lst[l:h]
	def binary_search_helper(lst, x, l, h):
		# List's low and high indices have criss-crossed
		# => x could not be found
		if l > h:
			return False

		mid = (l+h)/2
		if lst[mid] == x:
			return True
		elif x > lst[mid]:
			# search in second half of the sub-array
			return binary_search_helper(lst, x, mid+1, h)
		else:
			# search in first half of the sub-array
			return binary_search_helper(lst, x, l, mid-1)


	# Call the helper
	return binary_search_helper(lst, x, 0, len(lst)-1)



# Iterative implementation for binary search
def binary_search_i(lst, x):
	l, h = 0, len(lst)-1
	while l <= h:
		mid = (l+h)/2
		if lst[mid] == x:
			return True
		elif x > lst[mid]:
			# search in second half of the sub-array
			l = mid+1
		else:
			# search in first half of the sub-array
			h = mid-1

	# Couldn't find 'x'
	return False



# If not explicity called, use recursive binary search as default
binary_search = binary_search_r


def test_recursive_version():
	assert(binary_search([1,2,3,4,5], 2) == True)
	assert(binary_search([1,2,3,4,5], 1) == True)
	assert(binary_search([1,2,3,4,5], 5) == True)
	assert(binary_search([1,2,3,4,5], 0) == False)

	assert(binary_search((1,2,3,4,5), 5) == True)
	assert(binary_search((1,2,3,4,5), 0) == False)

	assert(binary_search([2,4,6, 8], 2) == True)
	assert(binary_search([2,4,6, 8], 4) == True)
	assert(binary_search([2,4,6, 8], 6) == True)
	assert(binary_search([2,4,6, 8], 8) == True)
	assert(binary_search([2,4,6, 8], 1) == False)
	assert(binary_search([2,4,6, 8], 3) == False)
	assert(binary_search([2,4,6, 8], 5) == False)
	assert(binary_search([2,4,6, 8], 7) == False)


def test_iterative_version():
	assert(binary_search_i([1,2,3,4,5], 2) == True)
	assert(binary_search_i([1,2,3,4,5], 1) == True)
	assert(binary_search_i([1,2,3,4,5], 5) == True)
	assert(binary_search_i([1,2,3,4,5], 0) == False)

	assert(binary_search_i((1,2,3,4,5), 5) == True)
	assert(binary_search_i((1,2,3,4,5), 0) == False)

	assert(binary_search_i([2,4,6, 8], 2) == True)
	assert(binary_search_i([2,4,6, 8], 4) == True)
	assert(binary_search_i([2,4,6, 8], 6) == True)
	assert(binary_search_i([2,4,6, 8], 8) == True)
	assert(binary_search_i([2,4,6, 8], 1) == False)
	assert(binary_search_i([2,4,6, 8], 3) == False)
	assert(binary_search_i([2,4,6, 8], 5) == False)
	assert(binary_search_i([2,4,6, 8], 7) == False)



# basic testcases
if __name__ == "__main__":
	test_recursive_version()
	test_iterative_version()

