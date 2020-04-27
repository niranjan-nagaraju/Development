'''
Heap sort:
	Build a max-heap out of the list
	remove top of the max-heap, moving to the end of the list,
	and working with the rest of the heap, until heap shrinks to 1 item
	At this point, the list would be sorted
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


	return a



if __name__ == '__main__':
	a = [5, 4, 2, 3, 6, 8, 9]
	heap_sort(a)
	assert a == [2,3,4,5,6,8,9]

