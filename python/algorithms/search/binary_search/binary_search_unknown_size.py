'''
Binary search for 'key' for list, lst
between the sub-array lst[l:h]

NOTE: both 'l' and 'h' might be well over the lst length (which is not known ahead of time, think of a live-stream)
'''
def binary_search_unknown_size(lst, key, l, h):
	while l <= h:
		mid = (l+h)/2

		try:
			# Try accessing a[mid]
			# If this results in an IndexError (ie mid > stream-size)
			# then halve the window for next iteration
			if lst[mid] == key:
				return mid
			elif key > lst[mid]:
				# key might be in second half
				l = mid + 1
			else: # key < lst[mid]
				# key might be in first half
				h = mid - 1
		except IndexError:
			# Error accessing a[mid]
			h = mid - 1

	return -1



if __name__ == '__main__':
	a = range(1, 110, 2)
	assert binary_search_unknown_size(a, 3, 0, 128) == 1
	assert binary_search_unknown_size(a, 70, 32, 64) == -1
	assert binary_search_unknown_size(a, 69, 32, 64) == 34
	assert binary_search_unknown_size(a, 33, 16, 32) == 16
	assert binary_search_unknown_size(a, 39, 16, 32) == 19
	assert binary_search_unknown_size(a, 65, 16, 32) == 32
	assert binary_search_unknown_size(a, 67, 0, 64) == 33
	assert binary_search_unknown_size(a, 4, 50, 128) == -1
	assert binary_search_unknown_size(a, 3, 50, 128) == -1 # 3 is at a[1]
	assert binary_search_unknown_size(a, 109, 30, 128) == 54

