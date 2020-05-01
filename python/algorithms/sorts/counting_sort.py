'''
Counting Sort Outline:
	Time complexity: O(n) if key space is narrow. {eg [0, k]}
	Count the number of times each key appears in the list,
	Iterate across key-space (min, max) an fill an auxiliary list in sorted order.

	NOTE: Store the records themselves in the 'count' table instead of their frequency
	      so that two objects, r1 and r2 has the same key but are different otherwise.
		  Stroring them in the order they appear would also ensure the counting sort algorithm is stable.
'''

from collections import defaultdict
def counting_sort(a, key=None):
	count_table = defaultdict(lambda: [])

	# If a key function is not specified
	# Use entire entry as key
	# NOTE: This wont work if entries are not integers
	if key is None:
		key = lambda x: x

	minimum = maximum = key(a[0])
	for x in a:
		k = key(x)
		count_table[k].append(x)

		# Also calculcate key-space {min, max}
		# while processing them
		if k < minimum:
			minimum = k
		elif k > maximum:
			maximum = k

	sorted_a = [None]*len(a)
	i = 0
	for k in xrange(minimum, maximum+1):
		entries = count_table[k]
		if entries != []:
			for x in entries:
				sorted_a[i] = x
				i += 1
	return sorted_a


if __name__ == '__main__':
	assert counting_sort([5,4,3,2,1]) == [1,2,3,4,5]
	assert counting_sort(
		[5, 1, 7, 3, 4, 2, 8]) == [1, 2, 3, 4, 5, 7, 8]

	assert counting_sort(
		[11, 9, 7, 5, 3, 2, 1]) == [1, 2, 3, 5, 7, 9, 11]

	assert counting_sort(
		[1, 2, 3, 4, 5, 6, 7]) == [1, 2, 3, 4, 5, 6, 7]

	assert counting_sort(
		[1, 3, 5, 7, 2, 4, 6]) == [1, 2, 3, 4, 5, 6, 7]

	assert counting_sort([5,3,1,9,8,2,4,7]) == [1,2,3,4,5,7,8,9]

	assert counting_sort([(81,2), (45,4), (67,3), (21,4), (11,2), (21,5), (67,6)], key=lambda(x,y):x) == [
			(11,2), (21,4), (21,5), (45,4), (67,3), (67,6), (81,2)]

	assert counting_sort([(81,2), (45,4), (67,3), (21,4), (11,2), (21,5), (67,6)], key=lambda(x,y):y) == [
			(81,2), (11,2), (67,3), (45,4), (21,4), (21,5), (67,6)]

	assert counting_sort([(81,2), (45,4), (67,3), (21,4), (11,2), (21,5), (67,30)], key=lambda(x,y):x+y) == [
			(11,2), (21,4), (21,5), (45,4), (67,3), (81,2), (67,30)]

