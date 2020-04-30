'''
Comparison counting sort:
	Count the number of elements for each A[i]
	   count[x]: # of elements in A <= x

Sample run 1:
	A: [60, 35, 81, 98, 14, 47]

	count: [0, 0, 0, 0, 0, 0]

	i:0, x = 60
	count: [3, 0, 1, 1, 0, 0]

	i: 1, x = 35
	count: [3, 1, 2, 2, 0, 1]

	i: 2, x = 81
	count: [3, 1, 4, 3, 0, 1]

	i: 3, x = 98
	count: [3, 1, 4, 5, 0, 1]

	i: 4, x = 14
	count: [3, 1, 4, 5, 0, 2]

	count: [3, 1, 4, 5, 0, 2]
	S: [14, 35, 47, 60, 81, 98]

Sample run 2:
	A: [60. 35, 81, 98, 14, 35']
	count: [0, 0, 0, 0, 0, 0]

	i: 0, x = 60
	count: [3, 0, 1, 1, 0, 0]

	i: 1, x = 35
	count: [3, 1, 2, 2, 0, 1]

	i: 2, x = 81
	count: [3, 1, 4, 3, 0, 1]

	i: 3, x = 98
	count: [3, 1, 4, 5, 0, 1]

	i: 4, x = 14
	count: [3, 1, 4, 5, 0, 2]

	count: [3, 1, 4, 5, 0, 2]
	S: [14, 35, 35', 60, 81, 98]  # In-place 35 is before 35' in the sorted list
'''
def comparison_counting_sort(a):
	n = len(a)
	count = [0]*n
	for i in xrange(n-1):
		for j in xrange(i+1, n):
			if a[i] <= a[j]:
				count[j] += 1
			else:
				count[i] += 1

	sorted_a = [0]*n
	for i in xrange(n):
		sorted_a[count[i]] = a[i]

	return sorted_a



if __name__ == '__main__':
	assert comparison_counting_sort([60, 35, 81, 98, 14, 47]) == [14, 35, 47, 60, 81, 98]
	assert comparison_counting_sort([60, 35, 81, 98, 14, 35]) == [14, 35, 35, 60, 81, 98]
	assert comparison_counting_sort([5, 4, 2, 3, 6, 8, 9]) == [2,3,4,5,6,8,9]

	class Pair:
		def __init__(self, a, b):
			self.a = a
			self.b = b

		def __cmp__(self, other):
			return cmp(self.a, other.a)

	# Comparison counting sort is 'stable'
	assert comparison_counting_sort([Pair(60,1), Pair(35,2), Pair(81,3), Pair(98,4), Pair(14,5), Pair(35,6)]) == [
			Pair(14,5), Pair(35,2), Pair(35,6), Pair(60,1), Pair(81,3), Pair(98,4)
			]

	assert comparison_counting_sort([Pair(35,1), Pair(35,2), Pair(35,3), Pair(35,4), Pair(35,5), Pair(35,6)]) == [
			Pair(35,1), Pair(35,2), Pair(35,3), Pair(35,4), Pair(35,5), Pair(35,6)
			]

