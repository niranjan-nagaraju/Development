''' 
Implement merge
  Merge two sorted lists into a bigger sorted list

Merge:
	1. Start with i=la, j=lb
	2.   Copy the smaller of A[i] vs B[j] into C until we run out of either A or B
	3.   In the end if either A or B still has items pending, Copy them into C as they will all be > current items in C
	     NOTE: Either of A or B will have items pending not both.
'''

# Merge two sorted lists a[la:ua], b[lb:ub] into a single sorted list, c[]
def merge(a, la, ua, b, lb, ub):
	i, j, k = la, lb, 0
	c = [None] * (ua-la + ub-lb)

	while i < ua and j < ub: 
		if a[i] < b[j]:
			c[k] = a[i]
			i += 1
		else:
			c[k] = b[j]
			j += 1
		k += 1

	# One list has been completely merged at this point
	# Copy the other list as-is, it should contain only greater elements
	# at this point
	while i < ua:
		c[k] = a[i]
		i += 1
		k += 1
	
	while j < ub:
		c[k] = b[j]
		j += 1
		k += 1

	return c	


if __name__ == '__main__':
	# Testcase 1
	assert (
			merge( [1, 3, 5, 7],
				   0, 4,
				   [2, 4, 6, 8],
				   0, 4)
			==
			[1, 2, 3, 4, 5, 6, 7, 8]
			)
	
	# Testcase 2
	assert (
			merge( [1, 5, 8],
				   0, 3,
				   [2, 4, 6, 9, 10, 11],
				   0, 6)
			== 
			[1, 2, 4, 5, 6, 8, 9, 10, 11]
			)

	# Testcase 3
	assert (
			merge( [1, 5, 8, 12, 13, 15],
				   0, 6,
				   [2, 4, 6, 9, 10, 11],
				   0, 6)
			== 
			[1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15]
			)

	# Testcase 4
	assert (
			merge( [1, 2, 3],
				   0, 3,
				   [4, 5, 6],
				   0, 3)
			==
			[1, 2, 3, 4, 5, 6]
			)

