#!/usr/bin/python

# Add the current subset to the powerset list ordered by size
def add_to_powerset(powerset, prefix):
	i = len(prefix)
	if prefix not in powerset[i]:
		powerset[i].append(prefix)


# Generate subsets starting with given prefix
# Generate all subsets that start at 'startindex' of lengths 1,2,..(n-startindex), and append them to 'prefix'
# e.g., generate_power_set_helper(_, [1..5], [], 0, 2)
# subsets:
#  => len 1: [1]
#  => len 2: [1,2]
#
# e.g., generate_power_set_helper(_, [1..5], [x], 0, 3)
# subsets:
#  => len 2: [x,1]
#  => len 3: [x,1,2], [x,1,3]
#  => len 4: [x,1,2,3]
def generate_power_set_helper(powerset, a, prefix, startindex, n):
	if (startindex == n):
		return 

	prefix.append(a[startindex])
	add_to_powerset(powerset, prefix[:])

	for i in range(startindex+1, n):
		generate_power_set_helper(powerset, a, prefix[:], i, n)
		add_to_powerset(powerset, prefix[:])



# Generate all subsets starting from a[0], a[1] .. a[n-1] and merge all of them into a list
def generate_power_set(a, n):
	# Initialize an empty 2D list to store subsets ordered by length
	# Lengths of subsets vary from 0 to n
	powerset = [[] for i in xrange(n+1)]
	subsets_selector = range(n)
	for i in xrange(n):
		generate_power_set_helper (powerset, subsets_selector[i:], [], 0, n-i)

	# Fill in actual items from the original set based on the subset masks
	for i in range(n+1):
		for x in powerset[i]:
			for k in range(i):
				x[k] = a[x[k]]

	# If the original set was a string, stringify the subsets too
	if isinstance(a, str):
		powerset_str = [[] for i in xrange(n+1)]
		powerset_str[0] = ""
		for i in range(1,n+1):
			for x in powerset[i]:
				powerset_str[i].append(''.join(x))
		return powerset_str

	return powerset




def basic_testcases():
	a = range(1, 6)
	subsets = [[] for _ in xrange(5)]
	generate_power_set_helper(subsets, a, [], 0, 1)
	assert(subsets == [[], [[1]], [], [], []])

	subsets = [[] for _ in xrange(5)]
	generate_power_set_helper(subsets, a, [], 1, 2)
	assert(subsets == [[], [[2]], [], [], []])

	subsets = [[] for _ in xrange(5)]
	generate_power_set_helper(subsets, a, [], 0, 2)
	assert(subsets == [[], [[1]], [[1,2]], [], []])

	subsets = [[] for _ in xrange(5)]
	generate_power_set_helper(subsets, a, [], 1, 3)
	assert(subsets == [[], [[2]], [[2, 3]], [], []])

	subsets = [[] for _ in xrange(5)]
	generate_power_set_helper(subsets, a, [], 0, 3)
	assert(subsets == [[], [[1]], [[1,2], [1, 3]], [[1,2,3]], []])

	subsets = [[] for _ in xrange(5)]
	generate_power_set_helper(subsets, a, ['x'], 0, 3)
	assert(subsets == [[], [], [['x', 1]], [['x',1,2], ['x',1, 3]], [['x',1,2,3]]])

	subsets = [[] for _ in xrange(6)]
	generate_power_set_helper(subsets, a, [], 0, 5)
	assert subsets[0] == []
	assert subsets[1] == [[1]]
	assert subsets[2] == [[1,2], [1,3], [1,4], [1,5]]
	assert subsets[3] == [[1,2,3], [1,2,4], [1,2,5], [1,3,4], [1,3,5], [1,4,5]]
	assert subsets[4] == [[1,2,3,4], [1,2,3,5], [1,2,4,5], [1,3,4,5]]
	assert subsets[5] == [[1,2,3,4,5]]

	subsets = [[] for _ in xrange(6)]
	generate_power_set_helper(subsets, a, [], 1, 5)
	[[], [[2]], [[2, 3], [2, 4], [2, 5]], [[2, 3, 4], [2, 3, 5], [2, 4, 5]], [[2, 3, 4, 5]], []]
	assert subsets[0] == []
	assert subsets[1] == [[2]]
	assert subsets[2] == [[2,3], [2,4], [2,5]]
	assert subsets[3] == [[2,3,4], [2,3,5], [2,4,5]]
	assert subsets[4] == [[2,3,4,5]]
	assert subsets[5] == []


	a = "abcde"
	assert(generate_power_set(a, len(a)) == ['', ['a', 'b', 'c', 'd', 'e'], ['ab', 'ac', 'ad', 'ae', 'bc', 'bd', 'be', 'cd', 'ce', 'de'], ['abc', 'abd', 'abe', 'acd', 'ace', 'ade', 'bcd', 'bce', 'bde', 'cde'], ['abcd', 'abce', 'abde', 'acde', 'bcde'], ['abcde']])

	assert generate_power_set("abc", 3) == ['', ['a', 'b', 'c'], ['ab', 'ac', 'bc'], ['abc']]
	assert generate_power_set(('a', 'b', 'c'), 3) == [[], [['a'], ['b'], ['c']], [['a', 'b'], ['a', 'c'], ['b', 'c']], [['a', 'b', 'c']]]

	assert generate_power_set([1,3,5,7], 4) == [[], [[1], [3], [5], [7]], [[1, 3], [1, 5], [1, 7], [3, 5], [3, 7], [5, 7]], [[1, 3, 5], [1, 3, 7], [1, 5, 7], [3, 5, 7]], [[1, 3, 5, 7]]]


if __name__ == "__main__":
	basic_testcases()


