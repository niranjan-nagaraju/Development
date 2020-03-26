'''
https://www.interviewbit.com/problems/max-distance/

Max Distance

Given an array A of integers, find the maximum of j - i subjected to the constraint of A[i] <= A[j].
If there is no solution possible, return -1.

Example :
	A : [3 5 4 2]
	Output : 2 
	for the pair (3, 4)
'''

'''
Solution Outline: O(n) time, O(n) space
	Use a non-increasing stack pushing items+index into the stack if stack top > current item.
	At the end of a single pass, stack top would contain the minimum element.
	  [NOTE: The minimum element might not be part of the solution, so we store all numbers greater than it underneath]
	Another pass from the right, reduce the stack removing items from the stack as long as they are <= current item.
	  Calculate distance (j-i) using current index as j and stack top as i
	  Return the max of all such distances


Sample run 1:
	A:  [3,5,4,2]


	Maximum: 0
	Stack: []
  
  Pass 1:
	item: 3,0
	push(3,0)
	stack: (3,0)

	item: 5,1
	5 > stacktop

	item: 4,2
	4 > stacktop

	item: 2,3
	2 < stacktop
	push(2,3)
	stack: (3,0) (2,3)
  
  Pass 2:
	j = 3, item = 2
	stacktop: 2,3 >= item
	distance = j-stacktop[1] = 3 - 3 = 0
	pop()
	stack: (3,0)
	item < stacktop? NO

	j = 2, item = 4
	4 >= 3
	distance = 2-0 = 2
	pop()

	stack: empty

  return 2


Sample run 2:
	A:  [2, 1, 4, 3, 6, 5]

	stack: []
	max: 0

	Pass 1:
	  push(2,0)
	  stack: (2,0)

	  1 <= stacktop
	  push(1,1)
	  stack: (2,0) (1,1)

	  4 > stacktop
	  3 > stacktop
	  6 > stacktop
	  5 > stacktop

	Pass 2:
	  j = 5, item = 5
	  item > stacktop  (5 > 1): pop()
	  distance = 5 - 1 = 4
	  max distance = 4
	  stack: (2,0)
	  item > stacktop (5 > 2): pop()
	  distance = 5-0 = 5
	  > max distance
	  max distance = 5
	  stack: empty
	
	return 5
'''

class Solution:
	def max_distance(self, A):
		max_d = 0
		stack = [(A[0],0)]

		# Pass 1
		for i in xrange(1,len(A)):
			if stack[-1][0] >= A[i]:
				stack.append((A[i],i))

		# Pass 2
		j = len(A)-1
		for j in xrange(len(A)-1, -1, -1):
			if not stack:
				break

			while stack and A[j] >= stack[-1][0]:
				item, i = stack.pop()
				distance = j - i
				if distance > max_d:
					max_d = distance

		return max_d


if __name__ == '__main__':
	s = Solution()
	assert s.max_distance([3,5,4,2]) == 2
	assert s.max_distance([3,1]) == 0
	assert s.max_distance([2,1,4,3,6,5]) == 5
	assert s.max_distance([6,5,4,4,4,4,4,1]) == 4
	assert s.max_distance([2]*7) == 6

