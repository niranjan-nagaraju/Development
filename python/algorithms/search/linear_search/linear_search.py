'''
Linear search on a list
'''

def linear_search(l, x):
	for item in l:
		if x == item:
			return True

	return False



# basic testcases
if __name__ == "__main__":
	assert(linear_search([1,2,3,4,5], 2) == True)
	assert(linear_search([1,2,3,4,5], 0) == False)

	assert(linear_search((1,2,3,4,5), 5) == True)
	assert(linear_search((1,2,3,4,5), 0) == False)
	
	assert(linear_search({1,2,3,4,5}, 3) == True)
	assert(linear_search({1,2,3,4,5}, 0) == False)

	assert(linear_search([(1,2),(2,3), (3,4), (4,5)], (2,3)) == True)
	assert(linear_search([(1,2),(2,3), (3,4), (4,5)], (2,1)) == False)
