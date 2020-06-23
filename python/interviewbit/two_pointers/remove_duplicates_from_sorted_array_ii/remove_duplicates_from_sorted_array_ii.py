'''
https://www.interviewbit.com/problems/remove-duplicates-from-sorted-array-ii/

Remove Duplicates from Sorted Array II

Remove Duplicates from Sorted Array

Given a sorted array, remove the duplicates in place such that each element can appear atmost twice and return the new length.

Do not allocate extra space for another array, you must do this in place with constant memory.

Note that even though we want you to return the new length, make sure to change the original array as well in place

For example,
Given input array A = [1,1,1,2],

Your function should return length = 3, and A is now [1,1,2].
'''

'''
Solution Outline:
   1. Use two pointers, both moving forward, Set freq = 0 (freq: current item's occurence count)
   2. Pointer i moves ahead to do a pass across the entire sorted array.
   3. Pointer j keeps track of an available slot for the next element that doesn't repeat.
   4. As long as Pointer j's data == Pointer i's data and freq > 2, Update freq and Keep Moving ahead i
       4.1 If freq < 2, Store j's data into i and move both i and j ahead.
       4.2 When Pointer j's data != Pointer i's data, Store j's data into i and move both ahead by 1.
   5. Return 'i' as the new length of the array with all the duplicates 'shrunk' from the sorted array.


Sample run:
	A: [1,1,1,2,3,3,3]

	i=0, j=1, freq = 1
	A[i] = A[0] = 1
	A[j] = A[1] = 1
	  freq = 2
	  i+=1 => i = 1
	  A[i] = A[j]
	  A: [1,1,1,2,3,3,3]

	i=1, j=2, freq = 2
	A[i] = A[1] = 1
	A[j] = A[2] = 1
	  freq = 2

	i=1, j=3, freq=2
	A[i] = A[1] = 1
	A[j] = A[3] = 2
	A[i] != A[j]
	  freq = 1
	  i += 1 = 2
	  A[i] = A[j]
	  A: [1,1,2,2,3,3,3]

	i=2, j=4, freq=1
	A[i] = A[2] = 2
	A[j] = A[4] = 3
	A[i] != A[j]
	  freq = 1
	  i += 1 = 3
	  A[i] = A[j]
	  A: [1,1,2,3,3,3,3]
	
	i=3, j=5, freq=1
	A[i] = A[3] = 3
	A[j] = A[5] = 3
	A[i] == A[j]
	  freq < 2 => freq = 2
	  i += 1 = 4
	  A[i] = A[j]
	  A: [1,1,2,3,3,3,3]

	i=4, j=6, freq=2
	A[i] = A[4] = 3
	A[j] = A[6] = 3
	A[i] == A[j]
	  freq == 2 =>skip

	j=7 == END
	return i+1 = 5 as the size of the array with duplicates more than f=2 removed.
	  A: [1,1,2,3,3,3,3]
	  A[:5] = [1,1,2,3,3]
'''

class Solution:
	def remove_duplicates_2(self, A):
		if not A:
			return 0

		freq = 1
		i = 0
		j = 1
		while j < len(A):
			if A[i] == A[j]:
				if freq < 2:
					freq += 1
					i += 1
					A[i] = A[j]
			else: # A[i] != A[j]
				freq = 1
				i += 1
				A[i] = A[j]
			j += 1

		return (i+1)


if __name__ == '__main__':
	s = Solution()
	assert s.remove_duplicates_2([]) == 0

	A = [1,1,2,3,3,3,3]
	assert s.remove_duplicates_2(A) == 5
	assert A[:5] == [1,1,2,3,3]

	A = [1,1,1,1,1]
	assert s.remove_duplicates_2(A) == 2
	assert A[:2] == [1,1]

	A = [1,1]
	assert s.remove_duplicates_2(A) == 2
	assert A[:2] == A

	assert s.remove_duplicates_2([1]) == 1
