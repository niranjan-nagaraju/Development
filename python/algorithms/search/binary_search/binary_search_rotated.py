'''
Binary search in a rotated array
'''


'''
Solution #1:
	1. Find a pivot in O(nlogn) time
	2. Then do binary search that accounts for indices that wrap around in a circular manner.
'''



'''
find_pivot:
	l = 0, h=n-1
	split at mid = (l+h)/2
	check a[0..mid] contains pivot or a[mid..h] contains the pivot
	  {A subarray a[x..y] contains the pivot if a[x] > a[y] (x<y)}
	Upon determining a subarray a[x..y] that contains the pivot, mark 'y' as a potential
	  and check a[x..y-1] again
'''
# Given a rotated sorted array, return the index
# where the rotation pivot is (i.e. the smallest item in the array)
def find_pivot(lst):
	# Convenience lambda function to return index which contains the minimim of lst[i] vs lst[j]
	minIndex = lambda i, j: i if lst[i] < lst[j] else j

	l, h = 0, len(lst)-1
	pivot_candidate = l
	while l <= h:
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
			# and narrow the window down to exclude either ends.
			if lst[l] == lst[h]:
				l = l+1
				h = h-1
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
			



def binary_search_rotated(lst, key):
	pass


if __name__ == '__main__':
	# sorted array could contain duplicates
	assert find_pivot([3,1,3]) == 1
	assert find_pivot([1,1]) == 0
	assert find_pivot([2,2,2,0,1]) == 3
	assert find_pivot([2,2,2,1,1]) == 4 # either of 3/4 should work here

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


