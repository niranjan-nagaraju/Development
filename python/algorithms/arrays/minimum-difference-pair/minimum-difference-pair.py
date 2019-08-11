#encoding: utf-8

'''
https://www.geeksforgeeks.org/find-minimum-difference-pair/

Find minimum difference between any two elements
Given an unsorted array, find the minimum difference between any pair in given array.

Examples :
	Input  : {1, 5, 3, 19, 18, 25};
	Output : 1
	Minimum difference is between 18 and 19

	Input  : {30, 5, 20, 9};
	Output : 4
	Minimum difference is between 5 and 9

	Input  : {1, 19, -4, 31, 38, 25, 100};
	Output : 5
	Minimum difference is between 1 and -4
'''


'''
Solution outline:
	Brute force solution is O(n²) gives us an upper bound.
	mindiff = None # None < all numbers (-ve and +ve)
	for i in 0 to n-1:
		for j in i+1 to n-1:
			diff = abs(array[i] - array[j])
			if mindiff < diff:
				mindiff = diff
				pair = (i, j)
	return mindiff

	Efficient solution:
	We need n² deltas because we can't conclude the minimum difference without looking at all pairs' differences.
	However, if the list is sorted, it becomes a number-line where we can immediately predict minimum difference from a given number.
	  - Just look at its neighbors.
	Sort using an O(nlogn) and then compute deltas between consecutive numbers, and the min amongst these deltas gives us the answer.
'''


def heap_sort(a):
	# Get left, right and parent indices for current index i
	left = lambda i: 2*i+1
	right = lambda i: 2*i+2
	parent = lambda i: (i-1)/2

	# Bubble-down an item at 'i'
	# to its rightful place, and the max-heap property is restored
	# Assumes left and right subtrees are already heaps.
	def bubble_down(i, n):
		l, r = left(i), right(i)
		larger_of = lambda i,j: i if a[i] > a[j] else j

		largest = i
		# Find the largest of left, right and root items
		if l < n:
			largest = larger_of(largest, l)

		if r < n:
			largest = larger_of(largest, r)

		# swap root of the subtree with the smallest of the left and right children
		if (largest != i):
			a[i], a[largest] = a[largest], a[i]
			bubble_down(largest, n)


	# Build a max-heap out of an unsorted list
	def build_max_heap():
		# heap[n/2-1 .. 0] are non-leaf nodes
		# bubble down all non-leaf nodes starting from the bottom-most leaf

		n = len(a)
		for i in xrange((n-2)/2, -1, -1):
			bubble_down(i, n)


	# Heap sort begins here #
	# Build an initial heap out of the array
	build_max_heap()

	# Keep removing top of the heap, to the end of the list
	# basically bubbling the max element first, followed by second-max, and so on
	# and work with the remainder of the heap, reducing heap size by 1
	# until the list is sorted
	for i in xrange(len(a)-1, -1, -1):
		a[0], a[i] = a[i], a[0]
		bubble_down(0, i)



def minimum_distance(a):
	heap_sort(a)
	min_dist = a[len(a)-1]
	pair = None
	for i in xrange(0, len(a)-1):
		if abs(a[i+1] - a[i]) < min_dist:
			min_dist = abs(a[i+1] - a[i])
			pair = a[i], a[i+1]
	return min_dist, pair


if __name__ == '__main__':
	assert minimum_distance([1, 5, 3, 19, 18, 25]) == (1, (18, 19))
	assert minimum_distance([30, 5, 20, 9]) == (4, (5, 9))
	assert minimum_distance([1, 19, -4, 31, 38, 25, 100]) == (5, (-4, 1))
