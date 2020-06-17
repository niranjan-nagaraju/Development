'''
https://www.interviewbit.com/problems/remove-duplicates-from-sorted-array/

Remove Duplicates from Sorted Array

Given a sorted array, remove the duplicates in place such that each element appears only once and return the new length.
Note that even though we want you to return the new length, make sure to change the original array as well in place
Do not allocate extra space for another array, you must do this in place with constant memory.

Example:
Given input array A = [1,1,2],
Your function should return length = 2, and A is now [1,2]. 
'''

'''
Solution Outline:
   1. Use two pointers, both moving forward.
   2. Pointer i moves ahead to do a pass across the entire sorted array.
   3. Pointer j keeps track of an available slot for the next element that doesn't repeat.
   4. As long as Pointer j's data == Pointer i's data, Keep Moving ahead i
       When Pointer j's data != Pointer i's data, Store j's data into i and move both ahead by 1.
   5. Return 'i' as the new length of the array with all the duplicates 'shrunk' from the sorted array.

Sample run:
  A: [1,1,1,2,2,3,3,4,5,5]
      0 1 2 3 4 5 6 7 8 9
  i=0, j = 1

  A[i] = A[0] = 1
  A[j] = A[1] = 1
  A[i] == A[j], j=2
  
  A[i] = A[0] = 1
  A[j] = A[2] = 1
  A[i] == A[j], j = 3

  A[i] = A[0] = 1
  A[j] = A[3] = 2
  A[i] != A[j]
   i += 1 = 1
   A[i] = A[j] = 2
  A: [1,2,1,2,2,3,3,4,5,5]
   j = 4

  A[i] = A[1] = 2
  A[j] = A[4] = 2
  A[i] == A[j]
  j = 5

  A[i] = A[1] = 2
  A[j] = A[5] = 3
  A[i] != A[j]
   i += 1 = 2
   A[i] = 3
  A: [1,2,3,2,2,3,3,4,5,5]
   j = 6
 
  A[i] = A[2] = 3
  A[j] = A[6] = 3
  A[i] == A[j]
   j = 7
 

  A[i] = A[2] = 3
  A[j] = A[7] = 4
  A[i] != A[j]
   i += 1 = 3
   A[i] = 4
  A: [1,2,3,4,2,3,3,4,5,5]
   j = 8
  
  A[i] = A[3] = 4
  A[j] = A[8] = 5
  A[i] != A[j]
   i += 1 = 4
   A[i] = 5
  A: [1,2,3,4,5,3,3,4,5,5]
   j = 9

  A[i] = A[3] = 5
  A[j] = A[9] = 5
  A[i] == A[j]
   j = 10
 
 END of array
 i = 4
 return 5 as the size of the new array with only unique elements
'''

class Solution:
	def remove_duplicates(self, A):
		if not A:
			return 0

		i = 0
		j = 1
		while j < len(A):
			if A[i] != A[j]:
				i += 1
				A[i] = A[j]
			j += 1

		return (i+1)


if __name__ == '__main__':
	s = Solution()
	A = [1,1,1,2,2,3,3,4,5,5]
	assert s.remove_duplicates(A) == 5
	assert A[:5] == [1,2,3,4,5]
			
	A = [1,2,3,4,5]
	assert s.remove_duplicates(A) == 5
	assert A[:5] == A

	A = [1]
	assert s.remove_duplicates(A) == 1
	assert A[:1] == A

	A = [500, 500, 500]
	assert s.remove_duplicates(A) == 1
	assert A[:1] == [500]
	
