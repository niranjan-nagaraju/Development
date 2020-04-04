'''
A majority element of a list is the element that appears more than n/2 times in the list.
n: length of the list
'''

'''
Solution 1: O(n logn)
	1. Sort the list
	2. If there's a majority element, it would be A[mid] {mid = (0+n-1)/2
	3. Scan left and right to find the frequency of A[mid], if its > n/2, return A[mid] as the majority element
'''
def majority_element_1(A):
	A.sort()

	n = len(A)

	mid = (n-1)/2
	candidate = A[mid]

	freq = 1
	i = mid-1
	while i >=0 and A[i] == candidate:
		i -= 1
	freq += mid-i-1

	i = mid+1
	while i < n and A[i] == candidate:
		i += 1
		freq += 1
		if freq > n/2:
			return candidate

	# No majority element in the list
	return -1



'''
Solution 2: O(n) time, O(n) memory
	Use a hash-table to store frequencies of all elements.
	Scan the hash-table, and find an element with > n/2 frequency.
'''
from collections import defaultdict
def majority_element_2(A):
	frequencies = defaultdict(lambda: 0)
	for x in A:
		frequencies[x] += 1

	for k in frequencies:
		if frequencies[k] > len(A)/2:
			return k

	return -1



'''
Solution 3: O(n) time, O(1) memory, Moore's voting algorithm
frequency(majority_element) > n/2
therefore frequency(majority_element) > combined frequencies(A[i]) 0<=i<n(A[i] != majority_element)
  Moore's voting algorithm:
    1. Keep a count for the candidate (initially candidate = A[0])
	2. Minus the count each time, the next A[i] is not the candidate, Increment if A[i] == candidate
		If the count becomes 0, Choose A[i] as the candidate and set count to 1
	3. Verify if candidate at the end of the pass is the majority element by counting its frequency in the second pass.

Sample run:
	A: [2,3,2,1,2]

	count: 1
	candidate: A[0] = 2

	A[1] != candidate
	count = 0
	candidate = 3
	count = 1

	A[2] != candidate
	count = 0
	candidate = 2
	count = 1

	A[3] != candidate
	count = 0
	candidate = 1
	count = 1

	A[4] != candidate
	count = 0
	candidate = 2
	count = 1

	frequency(2) == 3 > 5/2 => majority element = 2
'''
def majority_element_3(A):
	count = 1
	candidate = A[1]

	for i in xrange(1, len(A)):
		if A[i] != candidate:
			count -= 1
			if count == 0:
				candidate = A[i]
				count = 1
		else:
			count += 1

	count = 0
	for x in A:
		if x == candidate:
			count += 1
		if count > len(A)/2:
			return candidate

	return -1
	

majority_element = majority_element_3 # Use Moore's algorithm by default

if __name__ == '__main__':
	assert majority_element_1([2,3,2,1,2]) == 2
	assert majority_element_1([2,3,2,1,4]) == -1
	assert majority_element_1([3, 3, 4, 2, 4, 4, 2, 4, 4]) == 4

	assert majority_element_2([2,3,2,1,2]) == 2
	assert majority_element_2([2,3,2,1,4]) == -1
	assert majority_element_2([3, 3, 4, 2, 4, 4, 2, 4, 4]) == 4

	assert majority_element([2,3,2,1,2]) == 2
	assert majority_element([2,3,2,1,4]) == -1
	assert majority_element([3, 3, 4, 2, 4, 4, 2, 4, 4]) == 4

