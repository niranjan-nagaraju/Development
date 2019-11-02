'''
Bead sort implementation from geeksforgeeks
  at https://www.geeksforgeeks.org/bead-sort-natural-sorting-algorithm/
Translated to python from C implementation.
Credits: www.geeksforgeeks.org
'''

def bead_sort(a):
	n = len(a)
	maximum = max(a)
	beads = [[0]*maximum for _ in xrange(n)]

	# Set beads in n rows
	# Each row starts with beads (1s) equal to as many beads as a[i]
	# e.g.
	#  if a = [2,6,1,4,3]
	# beads = [
	#           [1, 1, 0, 0, 0, 0]
	#           [1, 1, 1, 1, 1, 1]
	#          	[1, 0, 0, 0, 0, 0]
	#          	[1, 1, 1, 1, 0, 0]
	#           [1, 1, 1, 0, 0, 0]
	#          ]
	for i in xrange(n):
		for j in xrange(a[i]):
			beads[i][j] = 1

	for j in xrange(maximum):
		# count number of beads to drop in each column
		drop_beads_by_column = 0
		for i in xrange(n):
			drop_beads_by_column += beads[i][j]

			# Take out all beads in current column
			beads[i][j] = 0

		# drop the beads from taken out from current column to lower levels
		# by stacking them one above the other
		for i in xrange(n-1, n-drop_beads_by_column-1, -1):
			beads[i][j] = 1

		
		# count the beads in each row (now sorted row-wise),
		# and add the count back into the original array
		# sorting the input array by ascending order
		# if a = [2,6,1,4,3]
		# post-sorting, the beads matrix(/the abacus) state would look-like
		# beads = [
		#           [1, 0, 0, 0, 0, 0]
		#           [1, 1, 0, 0, 0, 0]
		#          	[1, 1, 1, 0, 0, 0]
		#          	[1, 1, 1, 1, 0, 0]
		#           [1, 1, 1, 1, 1, 1]
		#          ]
		for i in xrange(n):
			# count number of 1s in each row
			num_beads_in_row = beads[i].count(1)
			a[i] = num_beads_in_row


	return a


if __name__ == '__main__':
	assert bead_sort([2,6,1,4,3]) == [1,2,3,4,6]
	assert bead_sort([5, 3, 1, 7, 4, 1, 1, 20]) == [1, 1, 1, 3, 4, 5, 7, 20]

