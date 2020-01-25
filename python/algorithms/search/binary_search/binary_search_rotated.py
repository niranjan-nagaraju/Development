'''
Binary search in a rotated array
'''


from find_pivot import find_pivot
from binary_search import binary_search

'''
Solution #1:
	1. Find a pivot in O(logn) time
	2. Then do binary search that accounts for indices that wrap around in a circular manner.
'''
def binary_search_rotated_1(lst, key):
	pivot = find_pivot(lst)

	n = len(lst)

	l, h = 0, n-1
	while l <= h:
		mid_ = (l+h)/2

		# adjust mid to shift by 'pivot' units
		mid = (mid_ + pivot) % n

		if lst[mid] == key:
			return mid
		elif key > lst[mid]:
			# key is in the right half
			l = (mid_ + 1)
		else: # key < lst[mid]
			# key is in the left half
			h = (mid_ - 1)

	# couldn't find key
	return -1




'''
Solution #2: O(log n)
  Observations:
	1. A sub-array a[l..h] is rotated if a[l] > a[h]
	2. If a sub-array a[l..h] is rotated, then a[l..h] wont contain
	   any key, x, between a[h] .. a[l], i.e. a[h] < x < a[l]


	Proceed as with a regular binary search
	Consider an initial window [l,h] -> [0, n-1]

	Therefore,
	  check if a[l..h] is rotated, if not run classic binary search over [l..h]
	  If a[l..h] is infact rotated,
	    split in the middle, mid = (l+h)/2
		  if a[l] > a[mid], i.e. a[l..mid] is rotated
		     check if key is between a[mid] to a[l], if it is, skip this half.
			 if the key is outside of a[mid] .. a[l], then find key in this sub-array
		  otherwise, (a[mid..h] is rotated)
		     check if key is between a[h] to a[mid], if it is, skip this half.
			 if the key is outside of a[h] .. a[mid], then find key in this sub-array

Sample run 1:
	a = [5,6,1,2,3,4]
	key: 2

	l, h = 0, 5
	mid = 2
	a[l] == 5 > a[h] == 4 => rotated
	a[l] > a[mid] == 1 => rotated
	  2 is between 1..5 => look in the other half
	  l = mid + 1 == 3

	l, h: 3, 5
	a[l] == 2 < a[h] == 4 => not rotated
	run binary_search(key=2, l=3, h=5)

Sample run 2:
	a = [6,7,8,9,10,1,2,4,5]
    key: 3

	l:0, h:8
	a[l] == 6 > a[h] == 5 => rotated
	mid = 4
	a[mid] == 10
	a[l] < a[mid] => a[mid] > a[h] (10 > 5)
	  is 3 between 5 .. 10? NO
	  => search key=3 could be within [mid..h] ==> [10,1,2,4,5]
	  l = mid + 1 == 5

	l, h: 5, 8
	mid: 6
	a[l] == 1 < a[h] == 5 => not rotated
	run regular binary search for key=3 in [5..8] => [1,2,4,5] ---> -1
'''
def binary_search_rotated_2(lst, key):
	l, h = 0, len(lst)-1

	while l <= h:
		mid = (l+h)/2

		if key == lst[mid]:
			return mid

		if lst[l] <= lst[h]:
			if lst[l] < lst[h]:
				# [l..h] is sorted, run traditional binary search
				# NOTE: while we can just do regular binary search comparisons here
				# adjusting l,h, mid, etc
				# the additional lst[l] < lst[h] will be avoided by calling the traditional binary search
				# that knows how to search when [l..h] is in proper sorted order.
				return binary_search(lst, key, l, h)

			# [l..h] is rotated
			# lst[l] == lst[h]
			# Found key
			if key == lst[l]:
				return l

			# Both the equal ends =/= key
			# skip them both from the search window
			h -= 1
			l += 1
		else:
			# [l..h] is rotated #

			# Left half of the array is rotated
			if lst[l] > lst[mid]:
				# [l..mid] is rotated
				# => if key is between lst[mid] .. lst[l], lst[mid] < key < lst[l]
				# then it wont be found in [l..mid]
				# e.g., [5,6,1,2], key = 4
				# 2 < 4 < 5, therefore 4 wont be in [5,6,1,2]
				if lst[mid] < key < lst[l]:
					l = mid + 1
				else: # key is in current half
					h = mid - 1
			else: # lst[mid] >= lst[h]
				# [mid..h] is rotated
				# => if key is between lst[h] .. lst[mid], lst[h] < key < lst[mid]
				# then it wont be found in [mid..h]
				# e.g., [5,6,1,2], key = 4
				# 2 < 4 < 5, therefore 4 wont be in [5,6,1,2]
				if lst[h] < key < lst[mid]:
					h = mid - 1
				else: # key is in current half
					l = mid + 1

	# Couldn't find key in a rotated sub-array
	return -1


# Use solution #2 by default
binary_search_rotated = binary_search_rotated_2

def test_binary_search_rotated(searchfn):
	assert searchfn([3,1], 1) == 1
	assert searchfn([1,3,1,1,1], 3) == 1

	assert find_pivot([4,5,6,7,1,2]) == 4
	assert searchfn([4,5,6,7,1,2], 1) == 4
	assert searchfn([4,5,6,7,1,2], 2) == 5
	assert searchfn([4,5,6,7,1,2], 0) == -1
	assert searchfn([4,5,6,7,1,2], 4) == 0
	assert searchfn([4,5,6,7,1,2], 5) == 1
	assert searchfn([4,5,6,7,1,2], 6) == 2
	assert searchfn([4,5,6,7,1,2], 7) == 3

	assert find_pivot([4,5,1,2,3]) == 2
	assert searchfn([4,5,1,2,3], 4) == 0
	assert searchfn([4,5,1,2,3], 5) == 1
	assert searchfn([4,5,1,2,3], 1) == 2
	assert searchfn([4,5,1,2,3], 2) == 3
	assert searchfn([4,5,1,2,3], 3) == 4
	assert searchfn([4,5,1,2,3], 6) == -1
	assert searchfn([4,5,1,2,3], 0) == -1

	assert find_pivot([3,1,3]) == 1
	assert searchfn([3,1,3], 0) == -1
	assert searchfn([3,1,3], 3) in (0,2)  # either of 0,2 will do
	assert searchfn([3,1,3], 1) == 1

	assert find_pivot([2,2,2,0,1]) == 3
	assert searchfn([2,2,2,0,1], 2) in (0,1,2) # either of 0,1,2 will do
	assert searchfn([2,2,2,0,1], 0) == 3
	assert searchfn([2,2,2,0,1], 1) == 4
	assert searchfn([2,2,2,0,1], -1) == -1
	assert searchfn([2,2,2,0,1], 3) == -1

	# Traditional binary search on an unrotated sorted array
	# should work just as well
	assert find_pivot([1,2,3,4,5,6,7]) == 0
	assert searchfn([1,2,3,4,5,6,7], 1) == 0
	assert searchfn([1,2,3,4,5,6,7], 2) == 1
	assert searchfn([1,2,3,4,5,6,7], 3) == 2
	assert searchfn([1,2,3,4,5,6,7], 4) == 3
	assert searchfn([1,2,3,4,5,6,7], 5) == 4
	assert searchfn([1,2,3,4,5,6,7], 6) == 5
	assert searchfn([1,2,3,4,5,6,7], 7) == 6

	assert find_pivot([2,5,6,0,0,1,2]) == 3
	assert searchfn([2,5,6,0,0,1,2], 0) in (3,4)  # either of 3/4
	assert searchfn([2,5,6,0,0,1,2], 2) in (0,6)  # either of 0 or 6 will do
	assert searchfn([2,5,6,0,0,1,2], 5) == 1
	assert searchfn([2,5,6,0,0,1,2], 1) == 5


if __name__ == '__main__':
	test_binary_search_rotated(binary_search_rotated_1)
	assert binary_search_rotated == binary_search_rotated_2
	test_binary_search_rotated(binary_search_rotated) # Use solution #2 by default

