#!/usr/bin/python


''' 
Implement merge
  Merge two sorted lists into a bigger sorted list
'''

def merge(a, b):
	i, j, k = 0, 0, 0
	c = []

	while (i < len(a)) and (j < len(b)): 
		if a[i] < b[j]:
			c.append(a[i])
			i += 1
		else:
			c.append(b[j])
			j += 1

	# One list has been completely merged at this point
	# Copy the other list as-is, it should contain only greater elements
	# at this point

	while i < len(a):
		c.append(a[i])
		i += 1
	
	while j < len(b):
		c.append(b[j])
		j += 1

	return c	


if __name__ == '__main__':
	# Testcase 1
	assert (
			merge( [1, 3, 5, 7],
				   [2, 4, 6, 8])
			==
			[1, 2, 3, 4, 5, 6, 7, 8]
			)
		
	# Testcase 2
	assert (
			merge( [1, 5, 8],
				   [2, 4, 6, 9, 10, 11])
			== 
			[1, 2, 4, 5, 6, 8, 9, 10, 11]
			)

	# Testcase 3
	assert (
			merge( [1, 5, 8, 12, 13, 15],
				   [2, 4, 6, 9, 10, 11])
			== 
			[1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15]
			)

	# Testcase 4
	assert (
			merge( [1, 2, 3],
				   [4, 5, 6])
			==
			[1, 2, 3, 4, 5, 6]
			)

