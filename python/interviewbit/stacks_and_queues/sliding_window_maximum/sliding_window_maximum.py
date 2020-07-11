'''
https://www.interviewbit.com/problems/sliding-window-maximum/

Sliding Window Maximum

Given an array of integers A. There is a sliding window of size B which
is moving from the very left of the array to the very right.
You can only see the w numbers in the window. Each time the sliding window moves
rightwards by one position. You have to find the maximum for each window.

The following example will give you more clarity.
The array A is [1 3 -1 -3 5 3 6 7], and B is 3.

Window position			Max
===================		===
[1 3 -1] -3 5 3 6 7		3
1 [3 -1 -3] 5 3 6 7		3
1 3 [-1 -3 5] 3 6 7		5
1 3 -1 [-3 5 3] 6 7		5
1 3 -1 -3 [5 3 6] 7		6
1 3 -1 -3 5 [3 6 7]		7
Return an array C, where C[i] is the maximum value of from A[i] to A[i+B-1].

Note: If B > length of the array, return 1 element with the max of the array.


Input Format
The first argument given is the integer array A.
The second argument given is the integer B.

Output Format
Return an array C, where C[i] is the maximum value of from A[i] to A[i+B-1]

For Example
Input 1:
    A = [1, 3, -1, -3, 5, 3, 6, 7]
    B = 3
Output 1:
    C = [3, 3, 5, 5, 6, 7]
'''


'''
Solution Outline:
	1. Use a deque with strictly-decreasing elements in queue order.
	2. Anytime a new item 'x' needs to be added to the current window,
		pop all items < x from the back of the queue
		Update C[] from the queue's front, as current window maximum.
		Then push x to the back of the queue
	3. Anytime an item 'x' moves out of the window, check if its also the queue's front (aka max of current window)
		If yes, pop it from the front of the queue, making way for the next maximum candidate in the window.


Sample run:
	A: [5, 1, 4, 2, 3], B = 3
	Deque: []
	C: []

	Initial window:
	Deque: [5]
	Deque: [5, 1]
	Deque: [5, 4] # pop(1), push(4)
	C: [5]

	slide:
	  [1,4,2]
	  prev-sow: 5 == q.front() => pop_front(5)
	  Deque: [4]
	  pop_back() until all < 2 are removed from the deque
	  Deque: [4]
	  push(2)
	  Deque: [4,2]
	  C: [5, 4]

	slide:
		[4,2,3]
		prev-sow: 1 != q.front()
		pop_back() all < 3
		Deque: [4]
		push(3)
		Deque: [4, 3]
		C: [5, 4, 4]
	
	slide:
		<not enough items>

	return [5, 4, 4]


Sample run 2:
    A = [1, 3, -1, -3, 5, 3, 6, 7]
    B = 3
	Deque: []
	C: []

	Initial window: [1, 3, -1]
		Deque: [1]
		x: 3,
			pop() < 3,
			push(3)
			Deque: [3]
		x: -1,
			pop() < -1,
			push(-1)
			Deque: [3, -1]
	C: [3]

	slide:
		[3, -1, -3]
		prev-sow: 1, x: -3
		q.front() != prev-sow
		pop() all < -3
		Deque: [3, -1]
		push(-3)
		Deque: [3, -1, -3]
		C: [3, 3]
	slide:
		[-1, -3, 5]
		prev-sow: 3, x: 5
		q.front() == prev-sow => q.pop_front()
		Deque: [-1, -3]
		pop() all < 5
		Deque: []
		push(5)
		Deque: [5]
		C: [3, 3, 5]
	slide:
		[-3, 5, 3]
		prev-sow: -1 != q.front()
		x: 3
		pop() all < 3
		Deque: [5]
		push(3)
		Deque: [5, 3]
		C: [3, 3, 5, 5]
	slide:
		[5, 3, 6]
		prev-sow: -3 != q.front()
		x: 6
		pop() all < 6
		Deque: []
		push(6)
		Deque: [6]
		C: [3, 3, 5, 5, 6]
	slide:
		[3,6,7]
		prev-sow: 5 != q,front()
		pop() all < 7
		Deque: []
		push(7)
		Deque: [7]
		C: [3, 3, 5, 5, 6, 7]
	slide:
		<out of array elements>
	
	return [3, 3, 5, 5, 6, 7]
'''

class Deque:
	def __init__(self):
		self.items = []

	def __len__(self):
		return len(self.items)

	def push_back(self, x):
		self.items.append(x)

	def pop_back(self):
		self.items.pop()

	def pop_front(self):
		self.items.pop(0)

	def front(self):
		return self.items[0]

	def back(self):
		return self.items[-1]


class Solution:
	def find_sliding_window_maximum(self, A, k):
		if k == 0:
			return []

		dq = Deque()

		# Initialize first window
		for i in xrange(k):
			try:
				while dq and dq.back() < A[i]:
					dq.pop_back()

				dq.push_back(A[i])
			except IndexError:
				# k > len(A)
				break

		swm = [dq.front()]

		# slide window to the right, 1 element at a time
		for i in xrange(k, len(A)):
			prev_sow = A[i-k]

			# If previous start-of-window was the window's maximum
			# remove it from the deque too
			if prev_sow == dq.front():
				dq.pop_front()

			while dq and dq.back() < A[i]:
				dq.pop_back()

			dq.push_back(A[i])
			swm.append(dq.front())

		return swm



if __name__ == '__main__':
	s = Solution()
	assert s.find_sliding_window_maximum([5,1,4,3,6,2], 8) == [6] # k > len(A)
	assert s.find_sliding_window_maximum([5,1,4,2,3], 3) == [5,4,4]
	assert s.find_sliding_window_maximum([1, 3, -1, -3, 5, 3, 6, 7], 3) == [3,3,5,5,6,7]
	assert s.find_sliding_window_maximum([5,1,4,3,6,2], 3) == [5,4,6,6]
	assert s.find_sliding_window_maximum([5,1,4,3,6,2], 4) == [5,6,6]
	assert s.find_sliding_window_maximum([5,1,4], 4) == [5] # k > len(A)
	assert s.find_sliding_window_maximum([5,1,4], 1) == [5, 1, 4]
	assert s.find_sliding_window_maximum([5,1,4], 2) == [5, 4]
	assert s.find_sliding_window_maximum([1,1,2,1,2,1], 3) == [2, 2, 2, 2]

	assert s.find_sliding_window_maximum([5,1,4,3], 2) == [5,4,4]
	assert s.find_sliding_window_maximum([4,3,2,1], 2) == [4,3,2]
	assert s.find_sliding_window_maximum([4,3,2,1], 3) == [4,3]
	assert s.find_sliding_window_maximum([9,10,9,-7,-4,-8,2,-6], 5) == [10, 10, 9, 2]
	assert s.find_sliding_window_maximum([1, -1], 1) == [1, -1]

