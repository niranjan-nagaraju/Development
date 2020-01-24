'''
Find the pivot of rotation (also the minimum element) in a rotated sorted array
     0 1 2 3 4 5 
a = [4,5,6,1,2,3]
pivot = 3

a = [7,2,4,5,5,6]
pivot = 1
'''

'''
Solution outline: O(log n)-ish
-----------------------------
O(n) if the number of repititions are ~n

find_pivot:
	l = 0, h=n-1
	split at mid = (l+h)/2
	check a[0..mid] contains pivot or a[mid..h] contains the pivot
	  {A subarray a[x..y] contains the pivot if a[x] > a[y] (x<y)}
	Upon determining a subarray a[x..y] that contains the pivot, mark 'y' as a potential
	  and check a[x..y-1] again


Sample run 1:
	A: [4,5,6,1,2,3]

	l: 0, h: 5
	mid: 2
	A[0] == 4 > A[5] == 3 => rotated
	  A[mid] = 6 > A[h] => pivot is in the right half, [6,1,2,3]
	  pivot could be 3 (index: 5)
	
	l: 3, h: 5
	mid: 4
	A[3] == 1 > A[5] == 3  => subarray [1,2,3] no longer rotated
	pivot = min(pivot, 1) == min(3, 1) == 1 (index: 3)
	we are done here


Sample run 2:
  A: [4,5,1,2,3]

  l: 0, h: 4
  mid: 2
  A[0] == 4 > A[4] == 3 => rotated
    A[mid] == 1 < A[l] == 4 => pivot is in the left half, [4,5,1]
	pivot candidate: 1

  l: 0, h: 1
  mid: 1
  A[0] == 4 > A[1] == 5 => no longer rotated
  pivot = min(4, pivot) == min(4, 1) == 1
  return 1


Sample run 3:
  A: [4,5,6,7,1,2]
  
  l: 0, h: 5
  mid: 2
  A[0] == 4 > A[5] == 2  => rotated
    A[mid] == 6 > A[h] == 2 => pivot is in the right half, [6,7,1,2]
	pivot candidate: 2

  l: 3, h: 5, [7,1,2]
  mid: 4
  A[l] == 7 > A[h] == 2  => rotated
    A[l == 7 > A[mid] == 1 => pivot is in the lef half, [7,1]
	pivot candidate: min(1, pivot) == min(1, 2) == 1 (index: 4)

  l: 5, h: 5
    END
'''
# Given a rotated sorted array, return the index
# where the rotation pivot is (i.e. the smallest item in the array)
def find_pivot(lst):
	# Convenience lambda function to return index which contains the minimim of lst[i] vs lst[j]
	minIndex = lambda i, j: i if lst[i] < lst[j] else j

	l, h = 0, len(lst)-1
	pivot_candidate = l
	while l <= h:
		# Down to last 2 elements
		# If they are both equql, return the first of the pair
		# as the pivot
		if l == h-1:
			if lst[l] == lst[h]:
				return l

		# Current window [l, h] is in sorted order
		# Check if the minimum element exists in lst[l]
		# If so, we are done here since we keep narrowing down the window
		# to contain pivot,
		# We either just moved past the pivot, or the pivot is in the current window
		#   If we had moved past, return last-saved pivot candidate
		#   If the current window has the pivot, it'd be lst[l] since lst[l..h] is in sorted order 
		if lst[l] <= lst[h]:
			pivot_candidate = minIndex(l, pivot_candidate)
			# If both ends of the window are equal, the pivot might still exist inside it
			# Check if either l,h can be a pivot candidate
			# and narrow the window down to exclude the left end.
			if lst[l] == lst[h]:
				l = l+1
				#h = h-1
			else:
				# lst[l] < lst[h]
				# Either l is the new minimum or we have already found the minimum
				#  In any case, return immediately
				break
		else: # Current window [l, h] is still rotated
			mid = (l+h)/2
			if lst[l] > lst[mid]:
				# pivot is in the left half
				pivot_candidate = mid
				h = mid-1
			elif lst[mid] > lst[h]:
				# pivot is in the right half
				pivot_candidate = h
				l = mid+1

	return pivot_candidate
			


if __name__ == '__main__':
	# sorted array could contain duplicates
	assert find_pivot([3,1,3]) == 1
	assert find_pivot([1,1]) == 0
	assert find_pivot([2,2,2,0,1]) == 3
	assert find_pivot([2,2,2,1,1]) == 3
	assert find_pivot([1,3,1,1]) == 2
	assert find_pivot([3,1,1,1]) == 1

	# Solution should work even if there aren't any duplicates
	# or if the sorted array wasn't rotated in the first place
	assert find_pivot([1]) == 0
	assert find_pivot([4,5,6,7,0,1,2]) == 4
	assert find_pivot([3,4,5,1,2]) == 3
	assert find_pivot([11,1,2,3,4,5,6,7,8,9,10]) == 1
	assert find_pivot([4,5,6,1,2,3]) == 3
	assert find_pivot([4,5,1,2,3]) == 2
	assert find_pivot([6,7,3,4,5]) == 2
	assert find_pivot([2,3,4,5,6]) == 0 # no rotation
	assert find_pivot([1,2,3,4,5,6]) == 0 # no rotation


