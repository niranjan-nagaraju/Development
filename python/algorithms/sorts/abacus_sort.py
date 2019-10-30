'''
Abacus sort / Bead sort/ Gravity sort (efficient O(n) space, O(mn) time) implementation
  n: size of the list
  m: max - min (in the list)
'''

'''
Sample run:
2 5 1 4

min: 1, max: 5
a: 1 1 1 1

i: 1 to 5
 j: 3 to 0


i: 1
  j: 3
   4 > 1
   a: 1 1 1 2
 
  j: 2
   1 > 1? NO

  j: 1
   5 > 1
   a: 1 1 2 2

  j: 0
   2 > 1
   a: 1 2 2 2


i: 2
 j: 3
  4 > 2
  a: 1 2 2 3

 j: 2
  1 > 2? NO
 
 j: 2
  5 > 2
  a: 1 2 3 3

 j: 0
  2 > 2? NO 


i: 3
  j: 3
   4 > 3
   a: 1 2 3 4

  j: 2
   1 > 3? NO

  j: 1
   5 > 3
   a: 1 2 4 4

  j: 0
   2 > 3? NO


i: 4
  j: 3
   4 > 4? NO

  j: 2
   1 > 4? NO

  j: 1
   5 > 4
   a: 1 2 4 5

  j: 0
   2 > 4? NO


i: 5
  j: 3
    4 > 5? NO

  j: 2
   1 > 5? NO

  j: 1
   5 > 5? NO

  j: 0
   2 > 5? NO


a: 1 2 4 5
'''

def abacus_sort(a):
	minimum, maximum = a[0], a[0]
	n = len(a)

	for x in a[1:]:
		# a[i] cannot be both maximum and minimum at the same time
		if x < minimum:
			minimum = x
		elif x > maximum:
			maximum = x

	# Initialize a temporary array filled with minimum
	temp = [minimum] * n
	for i in xrange(minimum, maximum):
		k = n-1
		for j in xrange(n-1, -1, -1):
			if a[j] > i:
				temp[k] += 1
				k -= 1

	# Copy temp array back into original array
	# replacing the array into sorted order
	for i in xrange(n):
		a[i] = temp[i]



if __name__ == '__main__':
	a = [2,5,1,4]
	abacus_sort(a)
	assert a == [1,2,4,5]

	b = [1, 2, 4, 2, 5, 3, 2, 3, 4]
	abacus_sort(b)
	assert b == [1,2,2,2,3,3,4,4,5]

