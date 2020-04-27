#!/usr/bin/python                                                               

''' 
Implement merge sort
'''

from merge import merge


def merge_sort(a):
	merge_sort_r(a, 0, len(a)-1)
	return a


# Copy all of list 'b' into 'a' starting at a['start_idx']
def copy_list(a, start_idx, b):
	i = start_idx
	for x in b:
		a[i] = x
		i += 1



# Recursive merge-sort
def merge_sort_r(a, lb, ub):
	if lb == ub:
		return

	mid = (lb + ub)/2

	merge_sort_r(a, lb, mid)
	merge_sort_r(a, mid+1, ub)

	tmp = merge(a, lb, mid+1, a, mid+1, ub+1)

	# Copy back merged sub-array into the original list, a[lb:ub]
	copy_list(a, lb, tmp)


if __name__ == '__main__':
	assert (merge_sort(
		[5, 1, 7, 3, 4, 2, 8]) == [1, 2, 3, 4, 5, 7, 8])

	assert (merge_sort(
		[11, 9, 7, 5, 3, 2, 1]) == [1, 2, 3, 5, 7, 9, 11])

	assert (merge_sort(
		[1, 2, 3, 4, 5, 6, 7]) == [1, 2, 3, 4, 5, 6, 7])

	assert (merge_sort(
		[1, 3, 5, 7, 2, 4, 6]) == [1, 2, 3, 4, 5, 6, 7])

