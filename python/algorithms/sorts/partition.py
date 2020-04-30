'''
Implement Partition for Quick-sort

Partition:
	Given an Array A[l:r], Use A[l] as pivot,
	Partition A[l:r] around pivot so all items < pivot are to the left, and those that are > pivot are to the right

Algorithm:
	1. Start with two pointers i=l+1, j=r
	2. Advance i as long as A[i] < pivot
	   Advance j as long as A[j] > pivot
	   Swap A[i] and A[j]
	   Repeat until i and j and don't cross each other
	3. j now represents the point where A[j] < pivot, and A[j+1] >= pivot
	   Swap A[j] and pivot => A[j] now is paritioned s.t
	      A[l..j-1] < pivot, A[j+1 .. r] >= pivot

Sample run:
	A: [5,3,1,9,8,2,4,7]
	l: 0
	r: 7

	A: [5, 3, 1, 9, 8, 2, 4, 7]
	    0  1  2  3  4  5  6  7

	pivot = A[l] = 5
	i = 1,
	  3 < 5, i = 2
	  1 < 5, i = 3
	  9 < 5? NO
	j = 7,
	  7 > 5, j = 6
	  4 > 5? NO
	swap(9, 4)
	A: [5, 3, 1, 4, 8, 2, 9, 7]
	    0  1  2  3  4  5  6  7

	i = 3,
	  4 < 5, i = 4
	  8 < 5? NO
	j = 6,
	  9 > 5, j = 5
	  2 > 5? NO
	swap(8, 2)
	A: [5, 3, 1, 4, 2, 8, 9, 7]
	    0  1  2  3  4  5  6  7

	i = 4,
	  2 < 5, i = 5
	  8 < 5? NO
	j = 5,
	  8 > 5, j = 4
	  2 > 5? NO
	
	i > j => Dont swap

  Finally, Swap (pivot, A[j])
  A: [2, 3, 1, 4, 5, 8, 9, 7]
	  0  1  2  3  4  5  6  7

  return j=4 as the partition point
'''
def partition(a, l, r):
	pivot = a[l]
	i, j = l+1, r
	while i <= j:
		while i<=r and a[i] < pivot:
			i += 1
		while j>=l and a[j] > pivot:
			j -= 1

		if i < j:
			a[i], a[j] = a[j], a[i]

	# Now the array A is partitioned around index j
	# swap A[j] with pivot(A[l]) so the array is partitioned around pivot
	a[l], a[j] = a[j], a[l]
	return j


if __name__ == '__main__':
	a = [5,3,1,9,8,2,4,7]
	assert partition(a, 0, len(a)-1) == 4
	assert a == [2,3,1,4,5,8,9,7]


