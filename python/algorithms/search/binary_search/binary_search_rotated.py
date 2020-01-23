'''
Binary search in a rotated array
'''


'''
Solution #1:
	1. Find a pivot in O(logn) time
	2. Then do binary search that accounts for indices that wrap around in a circular manner.
'''



from find_pivot import find_pivot

def binary_search_rotated(lst, key):
	pass


if __name__ == '__main__':
	assert find_pivot([4,5,6,7,1,2]) == 4

