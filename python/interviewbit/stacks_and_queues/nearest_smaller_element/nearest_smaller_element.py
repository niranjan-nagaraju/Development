'''
https://www.interviewbit.com/problems/nearest-smaller-element/

Nearest Smaller Element

Given an array, find the nearest smaller element G[i] for every element A[i] in the array such that the element has an index smaller than i.

More formally,
    G[i] for an element A[i] = an element A[j] such that 
    j is maximum possible AND 
    j < i AND
    A[j] < A[i]

Elements for which no smaller element exist, consider next smaller element as -1.

Input Format

The only argument given is integer array A.
Output Format

Return the integar array G such that G[i] contains nearest smaller number than A[i].If no such element occurs G[i] should be -1.
For Example

Input 1:
    A = [4, 5, 2, 10, 8]
Output 1:
    G = [-1, 4, -1, 2, 2]
Explaination 1:
    index 1: No element less than 4 in left of 4, G[1] = -1
    index 2: A[1] is only element less than A[2], G[2] = A[1]
    index 3: No element less than 2 in left of 2, G[3] = -1
    index 4: A[3] is nearest element which is less than A[4], G[4] = A[3]
    index 4: A[3] is nearest element which is less than A[5], G[5] = A[3]
    
Input 2:
    A = [3, 2, 1]
Output 2:
    [-1, -1, -1]
Explaination 2:
    index 1: No element less than 3 in left of 3, G[1] = -1
    index 2: No element less than 2 in left of 2, G[2] = -1
    index 3: No element less than 1 in left of 1, G[3] = -1
'''

'''
Solution Outline:
	0. Use a strictly-decreasing stack.
	1. Scan elements left to right,
	   1.1 for every element, x, pop everything from the stack that is >= x (as x will be a better minimum)
	   1.2 If there's a stack top, then mark it as x's nearest-smallest-element, otherwise x doesnt have an NSE.

Sample run:
	A: [5, 1, 4, 2, 3]
	stack: [5]
	nse: [-1]

	x: 1
	pop all >= 1 from the stack
	stack: []
	nse: [-1, -1]
	push(1)
	stack: [1]

	x: 4
	pop all >= 4 from the stack
	nse: [-1, -1, 1]
	push(4)
	stack: [4, 1]

	x: 2
	pop all >= 2 from the stack
	stack: [1]
	nse: [-1, -1, 1, 1]
	push(2)
	stack: [2, 1]

	x: 3
	pop all >= 3 from the stack
	stack: [2, 1]
	nse: [-1, -1, 1, 1, 2]
	push(3)
	stack: [3, 2, 1]

	return [-1, -1, 1, 1, 2]
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
	# @param A : list of integers
	# @return a list of integers
	def prevSmaller(self, A):
		s = Stack()
		nse = []

		for x in A:
			while s and s.top() >= x:
				s.pop()

			# Mark x's nse as stack top if it exists, else -1
			nse.append(s.top())

			s.push(x)

		return nse


if __name__ == '__main__':
	s = Solution()
	assert s.prevSmaller([1,2,3,4,5]) == [-1,1,2,3,4]
	assert s.prevSmaller([1,1,1,1,1]) == [-1]*5
	assert s.prevSmaller([4,5,2,10,8]) == [-1,4,-1,2,2]
	assert s.prevSmaller([3,2,1]) == [-1]*3
	assert s.prevSmaller([1,4,3,2,5]) == [-1, 1, 1, 1, 2]
	assert s.prevSmaller([5,1,4,2,3]) == [-1, -1, 1, 1, 2]


