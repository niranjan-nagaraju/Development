'''
NGE: NextGreaterElement
  find(): Finds next greater element, for each element, x, in the array.
          NGE is the first greater element on the right side of x in the array

  findCircular(): Same as find(), but treats the array as circular and continues the
         search by wrapping around from the left after hitting the end of the array.
'''
class NextGreaterElement(object):
	def __init__(self, nums):
		"""
		:type nums: List[int]
		:rtype: List[int]
		"""
		self.nums = nums

	'''
	Given an array, find the Next Greater Element (NGE) for every element.
	The Next greater Element for an element x is the first greater element on the right side of x in array.
	Elements for which no greater element exist, consider next greater element as -1.

	For the input array [4, 5, 2, 25}, the next greater elements for each element are as follows.

	Element --> NGE
	4 --> 5
	5 --> 25
	2 --> 25
	25 --> -1

	For the input array [13, 7, 6, 12}, the next greater elements for each element are as follows.
	Element --> NGE
	13 --> -1
	7 --> 12
	6 --> 12
	12 --> -1


	Sample Input
	4 5 2 25

	Sample Output
	[5,25,25,-1]

	Solution outline
	0. Initialize nge = [-1]*n
		nge : [-1, -1, -1, ..., -1]
	1. Use a stack and solve the problem of next-greater-element like matching parantheses.
	2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
		also record nge[x] = array[i]
	3. Push i onto stack	

	'''
	def find(self):
		self.stack = []
		nge = [-1] * len(self.nums)
		for i in xrange(len(self.nums)):
			while self.stack and self.nums[self.stack[0]] < self.nums[i]:
				x = self.stack.pop(0)
				nge[x] = self.nums[i]
			self.stack.insert(0, i)

		return nge

	'''
	Circular
	Given a circular array (the next element of the last element is the first element of the array), find the Next Greater Number for every element. The Next Greater Number of a number x is the first greater number to its traversing-order next in the array, which means you could search circularly to find its next greater number. If it doesn't exist, return  -1 for this number.

	Example 1:
	Input: [1,2,1]
	Output: [2,-1,2]
	Explanation: The first 1's next greater number is 2; 
	The number 2 can't find next greater number; 
	The second 1's next greater number needs to search circularly, which is also 2.

	Solution outline
		0. Initialize nge = [-1]*n
			nge : [-1, -1, -1, ..., -1]
		1. Use a stack and solve the problem of next-greater-element like matching parantheses.
		2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
			also record nge[x] = array[i]
		3. Push i onto stack
		4. Since its a circular array, traverse the array again (after reaching the end), until the penultimate item.
		   a = [1,2,3,4]
		   1,2,3,4,1,2,3 covers the circular array.
		5. Fill nge[x] only if it has not been found yet.
		   This ensures that a previously found nge[] is  not overwritten by a new one in the circular traversal,
		   as the new one is certainly 'farther'.
			e.g. [5,1,2,3,4]
			nge[1] should not be overwritten by 5, and should remain 2
	'''
	def findCircular(self):
		# Find linear next greater elements
		# then fill the ones that are still -1, in a circular traversal
		# by starting back at 0 and continuing the stack state from previous linear find
		nge = self.find()

		# Circular traversal, traverse till the penultimate item this time
		for i in xrange(len(self.nums)-1):
			while self.stack and self.nums[self.stack[0]] < self.nums[i]:
				x = self.stack.pop(0)
				# if NGE has already been found for the current item
				# Ignore it for this pass
				if nge[x] == -1:
					nge[x] = self.nums[i]
			self.stack.insert(0, i)

		return nge


if __name__ == '__main__':
	n = NextGreaterElement([1,2,1])
	assert n.find() == [2,-1,-1]
	assert n.findCircular() == [2,-1,2]

	n = NextGreaterElement([1,2,3,4])
	assert n.find() == [2,3,4,-1]
	assert n.findCircular() == [2,3,4,-1]

	n = NextGreaterElement([3,1,2,4])
	assert n.find() == [4,2,4,-1]
	assert n.findCircular() == [4,2,4,-1]

	n = NextGreaterElement([4,5,2,25])
	assert n.find() == [5,25,25,-1]
	assert n.findCircular() == [5,25,25,-1]

	n = NextGreaterElement([13,7,6,12])
	assert n.find() == [-1,12,12,-1]
	assert n.findCircular() == [-1,12,12,13]

	n = NextGreaterElement([4,3,2,1])
	assert n.find() == [-1, -1, -1, -1]
	assert n.findCircular() == [-1,4,4,4]


