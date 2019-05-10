'''
Insertion sort:
	At any given point, i, all items to the left are sorted.
	Insert a[i] in the right place, so now a[0..i] are all sorted.

	e.g.
	A: 2 4 5,   3 . .
	i: 3, A[i] = 3
	j = 2, 3 < 5, Copy 5 to A[3], A: 2 4 5 5
	j = 1, 3 < 4, Copy 4 to A[2], A: 2 4 4 5
	j = 0, 3 < 2? NO, A[1] = 3, A: 2 3 4 5
'''

def insertion_sort(a):
	n = len(a)
	for i in xrange(1, n):
		current = a[i]
		j = i - 1
		while j >= 0 and current < a[j]:
			a[j+1] = a[j]
			j -= 1
		a[j+1] = current

	return a


if __name__ == '__main__':
	a = [5,2,6,4,1,3]
	assert insertion_sort(a) == a == range(1,7)
	assert insertion_sort(range(5, 0, -1)) == range(1, 6)
	assert insertion_sort(range(1, 6)) == range(1, 6)
	assert (insertion_sort(
		[5, 1, 7, 3, 4, 2, 8]) == [1, 2, 3, 4, 5, 7, 8])

	assert (insertion_sort(
		[11, 9, 7, 5, 3, 2, 1]) == [1, 2, 3, 5, 7, 9, 11])

	assert (insertion_sort(
		[1, 2, 3, 4, 5, 6, 7]) == [1, 2, 3, 4, 5, 6, 7])

	assert (insertion_sort(
		[1, 3, 5, 7, 2, 4, 6]) == [1, 2, 3, 4, 5, 6, 7])

