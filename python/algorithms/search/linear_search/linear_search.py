'''
Linear search on a list
'''

# iterative linear search
# NOTE: sets do not support indexing, 
# and we want linear search working on sets too,
# so we manually count the index where we find a match
def linear_search(l, x):
	i = 0
	try:
		for item in l:
			if x == item:
				return i
			i += 1
	except IndexError:
		return -1

	return -1


# recursive linear search
def linear_search_r(l, x):
	def linear_search_helper(l, x, it, idx=0):
		try:
			if x == next(it):
				return idx
			return linear_search_helper(l, x, it, idx+1)
		except StopIteration:
			return -1

	return linear_search_helper(l, x, iter(l))



# basic testcases
def test_linear_search(search_f):
	assert(search_f([1,2,3,4,5], 2) == 1)
	assert(search_f([1,2,3,4,5], 0) == -1)

	assert(search_f((1,2,3,4,5), 5) == 4)
	assert(search_f((1,2,3,4,5), 0) == -1)

	assert(search_f({1,2,3,4,5}, 3) == 2)
	assert(search_f({1,2,3,4,5}, 0) == -1)

	assert(search_f([(1,2),(2,3), (3,4), (4,5)], (2,3)) == 1)
	assert(search_f([(1,2),(2,3), (3,4), (4,5)], (2,1)) == -1)



if __name__ == '__main__':
	test_linear_search(linear_search)
	test_linear_search(linear_search_r)

