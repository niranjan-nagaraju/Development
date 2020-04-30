''' 
Implement quick sort
'''

from partition import partition


def quick_sort(a):
	quick_sort_r(a, 0, len(a)-1)
	return a



# Recursive quick-sort
def quick_sort_r(a, lb, ub):
	if lb < ub:
		partition_point = partition(a, lb, ub)
		quick_sort_r(a, lb, partition_point-1)
		quick_sort_r(a, partition_point+1, ub)

	return a



if __name__ == '__main__':
	assert quick_sort(
		[5, 1, 7, 3, 4, 2, 8]) == [1, 2, 3, 4, 5, 7, 8]

	assert quick_sort(
		[11, 9, 7, 5, 3, 2, 1]) == [1, 2, 3, 5, 7, 9, 11]

	assert quick_sort(
		[1, 2, 3, 4, 5, 6, 7]) == [1, 2, 3, 4, 5, 6, 7]

	assert quick_sort(
		[1, 3, 5, 7, 2, 4, 6]) == [1, 2, 3, 4, 5, 6, 7]

	assert quick_sort([5,3,1,9,8,2,4,7]) == [1,2,3,4,5,7,8,9]

