'''
https://leetcode.com/problems/find-first-and-last-position-of-element-in-sorted-array/

34. Find First and Last Position of Element in Sorted Array

Given an array of integers nums sorted in ascending order, find the starting and ending position of a given target value.
Your algorithm's runtime complexity must be in the order of O(log n).
If the target is not found in the array, return [-1, -1].

Example 1:
Input: nums = [5,7,7,8,8,10], target = 8
Output: [3,4]

Example 2:
Input: nums = [5,7,7,8,8,10], target = 6
Output: [-1,-1]
'''


'''
Solution Outline (Iterative version):
	1. Do a regular binary search and find a matched position for the element.
       We don't yet know if this is the first, last or only occurence (making it both the first, last)
	2. Once such a position is found, we do findLeftMost(0, position-1), as well as findRightMost(position+1, n-1)
       to find the first and the last occurences, if the previous binary search yield no match, we return -1, -1
	3. findLeftMost(l, h, position):  {position previously matched for element, we'll try to find a replacement for it}
		Tries to find a smaller position than previous match repeatedly by looking in the left or right subarray.
		Split array[l:h] in the middle at (l+h)/2
		if l > h:
   			array[l:h] does not contain element, just return previous position because we cannot do better.
			This will ensure min(position, findLeftMost()) will return the better of current and new positions,
			without needing a bunch of if/else's.
		if array[mid] == element, then find element in the left subarray
			position = mid
			h = mid - 1
		else {array[mid] != element => array[mid] < element, look for element in the right subarray}
			l = mid + 1
		Repeat until l > h, and return position
	4. findRightMost(l, h, position):  {position previously matched for element, we'll try to find a replacement for it}
		findRightMost() is similar to findLeftMost() but is right-leaning and tries to find the right most occurence of element.
		Tries to find a bigger position than previous match repeatedly by looking in the left or right subarray.
		Split array[l:h] in the middle at (l+h)/2
		if l > h:
   			array[l:h] does not contain element, just return previous position because we cannot do better.
			This will ensure max(position, findRightMost()) will return the better of current and new positions,
			without needing a bunch of if/else's.
		if array[mid] == element, then find element in the right subarray
			position = mid
			l = mid + 1
		else {array[mid] != element => array[mid] > element, look for element in the left subarray}
			h = mid - 1
		Repeat until l > h, and return position
	
Sample run:
Input: nums = [1,1,2,2,2,2,2,3,3,3], target = 2
       0 1 2 3 4 5 6 7 8 9
Input: 1 1 2 2 2 2 2 3 3 3

binSearch(nums, 8) == 4

findLeftMost(0, 3, 4):
   mid = (0+3)/2 == 1
   a[mid] != 2 => search in right sub-array
   l = mid + 1 = 2
   > l = 2, h = 3, position = 4
      mid = (2+3)/2 == 2
      a[mid] = a[2] == 2 => search in left sub-array, but take note that mid is the new best position
	  position = 2
	  h = mid - 1 == 1
         > l, h, position = 2, 1, 2
           l > h: return 2 as the leftmost position
           return 2
   return 2
         


       0 1 2 3 4 5 6 7 8 9
Input: 1 1 2 2 2 2 2 3 3 3
position = 4

findRightMost(5, 9, 4):
   l, h, position = 5, 9, 4
   mid = (5+9)/2 == 7
   a[mid] == a[7] = 3 != 2 => search in left sub-array
   h = mid - 1 = 6
   > l, h, position = (5, 6, 4):
     mid = (5+6)/2 == 5
     a[mid] == a[5] = 2 == 2 => search in right sub-array, but take note that mid is the new best position
	 position = 5
	 l = mid + 1 == 6
     > l, h, position = (6, 6, 5):
       mid = (6+6)/2 == 6
       a[mid] == a[6] == 2 => search in right sub-array, but take note that mid is the new best position
	   position = 6
	   l = mid + 1 == 7
       > l, h, position = (7, 6, 6):
         l > h: return 6 as the rightmost position
   return 6

return (2,6)
'''

from binary_search import binary_search_i as binary_search

# Returns the first and last occurence of 'element' in array
def findFirstLast(array, element):
	match = binary_search(array, element)

	if match == -1:
		return (-1, -1)

	left = findLeftMost(array, element, 0, match-1, match)
	right = findRightMost(array, element, match+1, len(array)-1, match)
	return (left, right)


# Find leftmost occurence of 'element' in array[l:h]
def findLeftMost(array, element, l, h, position):
	while l <= h:
		mid = (l+h)/2
		if array[mid] == element:
			# search to the left of the sub-array
			# after recording 'mid' as a better match
			# compared to 'position'
			position = mid
			h = mid - 1
		else:
			# search to the right of the sub-array
			l = mid + 1

	return position


# Find rightmost occurence of 'element' in array[l:h]
def findRightMost(array, element, l, h, position):
	while l <= h:
		mid = (l+h)/2
		if array[mid] == element:
			# search to the right of the sub-array
			# after recording 'mid' as a better match
			# compared to 'position'
			position = mid
			l = mid + 1
		else:
			# search to the left of the sub-array
			h = mid - 1
	return position

	

	
if __name__ == '__main__':
	assert binary_search([1,1,2,2,2,2,2,3,3,3], 2) == 4

	assert findLeftMost([1,1,2,2,2,2,2,3,3,3], 2, 0, 9, 9) == 2
	assert findRightMost([1,1,2,2,2,2,2,3,3,3], 2, 0, 9, 0) == 6

	assert findLeftMost([1,1,2,2,2,2,2,3,3,3], 2, 0, 3, 4) == 2
	assert findRightMost([1,1,2,2,2,2,2,3,3,3], 2, 5, 9, 4) == 6

	assert findFirstLast([1,1,2,2,2,2,2,3,3,3], 2) == (2,6)
	assert findFirstLast([5,7,7,8,8,10], 8) == (3,4)
	assert findFirstLast([5,7,7,8,8,10], 7) == (1,2)
	assert findFirstLast([5,7,7,8,8,10], 6) == (-1,-1)
	assert findFirstLast([5,7,7,8,8,10], 5) == (0,0)
	assert findFirstLast([5,7,7,8,8,10], 10) == (5,5)

