#encoding: utf-8

'''
https://www.interviewbit.com/problems/maxspprod/

MAXSPPROD

You are given an array A containing N integers. The special product of each ith integer in this array is defined as the product of the following:

LeftSpecialValue: For an index i, it is defined as the index j such that A[j]>A[i] (i>j). If multiple A[j]’s are present in multiple positions, the LeftSpecialValue is the maximum value of j.

RightSpecialValue: For an index i, it is defined as the index j such that A[j]>A[i] (j>i). If multiple A[j]s are present in multiple positions, the RightSpecialValue is the minimum value of j.

Write a program to find the maximum special product of any integer in the array.

Input: You will receive array of integers as argument to function.

Return: Maximum special product of any integer in the array modulo 1000000007.

Note: If j does not exist, the LeftSpecialValue and RightSpecialValue are considered to be 0.

Constraints 1 <= N <= 10^5 1 <= A[i] <= 10^9
'''

'''
Solution Outline:

A: [ 5, 9, 6, 8, 6, 4, 6, 9, 5, 4, 9 ]

         0  1  2  3  4  5  6  7  8  9  10
       [ 5, 9, 6, 8, 6, 4, 6, 9, 5, 4, 9 ]
left:  [ 0, 0, 9, 9, 8, 6, 8, 0, 9, 5, 0 ]
idx    [ 0, 0, 1, 1, 3, 4, 3, 0, 7, 8, 0 ]
right: [ 9, 0, 8, 9, 9, 6, 9, 0, 9, 9, 0 ]
idx    [ 1, 0, 3, 7, 7, 6, 7, 0, 10,10,0 ] 

prod:  [ 0, 0, 3, 7, 21,24,21,0, 70,80,0 ]
max: 80

To get left special value:
	Previous greater element
	 Use a stack, push items replacing stack items which are lesser than current.
	 Mark current item's PGE is Stack top, if stack is empty => -1
	 A: [2,1,4,3]
	 stack: []

	 2,
	 PGE: [-1] (stack is empty)
	 stack: [2]

	 1,
	 PGE: [-1, 2]
	 stack: [1,2]

	 4,
	 replace < 4
	 PGE: [-1, 2, -1]
	 stack: [4]

	 3,
	 replace < 3
	 stack: [3,4]
	 PGE: [-1, 2, -1, 4]


To get right special value:
	Next Greater element
	 Use a non-increasing stack, push items replacing current stack items which are lesser than current
	 Mark NGE of stack items as current element
	 A: [2,1,4,3]
	 NGE: [-1,-1,-1,-1]
	 stack: []

	 2,
	 stack: [2]

	 1,
	 stack: [1,2]

	 4,
	 > top
	 NGE: [4,4,-1,-1]
	 stack: [4]

	 3,
	 stack: [3,4]
	 NGE: [4,4,-1,-1]
'''

class Stack:
	def __init__(self):
		self.items = []

	def __len__(self):
		return len(self.items)

	def push(self, x):
		self.items.append(x)

	def pop(self):
		return self.items.pop()

	def top(self):
		return self.items[-1] if self.items else -1


class Solution:
	def maxSpecialProduct(self, A):
		if not A:
			return 0

		s1 = Stack()
		s2 = Stack()

		n = len(A)

		PGE = [0]*n
		NGE = [0]*n

		for i in xrange(len(A)):
			# PGE stack
			while s1 and A[s1.top()] <= A[i]:
				x = s1.pop()

			# The stack is now rid of all items <= A[i]
			PGE[i] = s1.top() if s1 else 0
			s1.push(i)

			# NGE stack
			while s2 and A[s2.top()] < A[i]:
				x = s2.pop()
				# Mark current element as NGE for all the popped items
				NGE[x] = i

			s2.push(i)

		# print PGE, NGE, map(lambda x,y: x*y, PGE, NGE)
		return max(map(lambda x,y: x*y, PGE, NGE)) % 1000000007


if __name__ == '__main__':
	s = Solution()
	assert s.maxSpecialProduct([2,1,4,3]) == 0
	# PGE: [0, 0, 0, 2]
	# NGE: [2, 2, 0, 0])
	# prd: [0, 0, 0, 0] 

	assert s.maxSpecialProduct([ 5, 9, 6, 8, 6, 4, 6, 9, 5, 4, 9 ]) == 80
	# PGE: [0, 0, 1, 1, 3, 4, 3, 0, 7, 8, 0]
	# NGE: [1, 0, 3, 7, 7, 6, 7, 0, 10, 10, 0]
	# prd: [0, 0, 3, 7, 21, 24, 21, 0, 70, 80, 0]

	assert s.maxSpecialProduct([ 7, 5, 7, 9, 8, 7 ]) == 0
	# PGE: [0, 0, 0, 0, 3, 4]
	# NGE: [3, 2, 3, 0, 0, 0]
	# prd: [0, 0, 0, 0, 0, 0]

