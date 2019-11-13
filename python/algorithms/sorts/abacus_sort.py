'''
Abacus sort / Bead sort/ Gravity sort (efficient O(n) space, O(mn) time) implementation
  n: size of the list
  m: max - min (in the list)
'''


'''
Version 1: Scan input array R-L so the temp array is sorted in ascending order
Sample run:
a[]: 2 5 1 4

min: 1, max: 5

Initialize temp[] with min
temp:
      +---------------+
      | 1 | 1 | 1 | 1 |
      +---------------+

Iterate
i: min(1) to max-1(4)
 j: 3 to 0 (R-L)


i: 1
                    j,k
      +---------------+
      | 1 | 1 | 1 | 1 |
      +---------------+

  j: 3
   a[j] = 4 > 1
               j,k  
      +---------------+
      | 1 | 1 | 1 | 2 |
      +---------------+

  j: 2
   a[j] > 1? NO
            j   k  
      +---------------+
      | 1 | 1 | 1 | 2 |
      +---------------+

  j: 1
   a[j] = 5 > 1
        j   k  
      +---------------+
      | 1 | 1 | 2 | 2 |
      +---------------+

  j: 0
   a[j] = 2 > 1
   temp: 1 2 2 2
        k    
      +---------------+
      | 1 | 2 | 2 | 2 |
      +---------------+


i: 2
                    j,k
      +---------------+
      | 1 | 2 | 2 | 2 |
      +---------------+

 j: 3
  a[j] = 4 > 2
               j,k
      +---------------+
      | 1 | 2 | 2 | 3 |
      +---------------+

 j: 2
  a[j] = 1 > 2? NO
            j   k
      +---------------+
      | 1 | 2 | 2 | 3 |
      +---------------+

 
 j: 1
  a[j] = 5 > 2
        j   k
      +---------------+
      | 1 | 2 | 3 | 3 |
      +---------------+

 j: 0
  a[j] = 2 > 2? NO 
            k   
      +---------------+
      | 1 | 2 | 3 | 3 |
      +---------------+



i: 3
                    j,k
      +---------------+
      | 1 | 2 | 3 | 3 |
      +---------------+

  j: 3
   a[j] = 4 > 3
               j,k
      +---------------+
      | 1 | 2 | 3 | 4 |
      +---------------+

  j: 2
   a[j] = 1 > 3? NO
            j   k
      +---------------+
      | 1 | 2 | 3 | 4 |
      +---------------+

  j: 1
   a[j] = 5 > 3
        j   k   
      +---------------+
      | 1 | 2 | 4 | 4 |
      +---------------+

  j: 0
   a[j] = 2 > 3? NO
            k   
      +---------------+
      | 1 | 2 | 4 | 4 |
      +---------------+


i: 4
                   j,k   
      +---------------+
      | 1 | 2 | 4 | 4 |
      +---------------+

  j: 3
   a[j] = 4 > 4? NO
                j   k   
      +---------------+
      | 1 | 2 | 4 | 4 |
      +---------------+


  j: 2
   a[j] = 1 > 4? NO
            j       k   
      +---------------+
      | 1 | 2 | 4 | 4 |
      +---------------+

  j: 1
   a[j] = 5 > 4
        j       k   
      +---------------+
      | 1 | 2 | 4 | 5 |
      +---------------+

  j: 0
   2 > 4? NO
                k   
      +---------------+
      | 1 | 2 | 4 | 5 |
      +---------------+

temp: 1 2 4 5
copy into a: [1,2,4,5]
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


	return a


'''
Version 2:
	Scans input array L-R and the resulting temporary sorted array will be in decreasing order.


Sample run:
2 5 1 4

min: 1, max: 5
temp: 1 1 1 1

i: 1 to max-1(4)
 j: 0 to 3


i: 1
  j: 0
   2 > 1
   temp: 2 1 1 1
 
  j: 1
   5 > 1
   temp: 2 2 1 1

  j: 2
   1 > 1? NO

  j: 3
   4 > 1
   temp: 2 2 2 1

i: 2
  j: 0
   2 > 2? NO
 
  j: 1
   5 > 2
   temp: 3 2 2 1

  j: 2
   1 > 2? NO

  j: 3
   4 > 2
   temp: 3 3 2 1


i: 3
  j: 0
   2 > 3? NO
 
  j: 1
   5 > 3
   temp: 4 3 2 1

  j: 2
   1 > 3? NO

  j: 3
   4 > 3
   temp: 4 4 2 1


i: 4
  j: 0
   2 > 4? NO
 
  j: 1
   5 > 4
   temp: 5 4 2 1

  j: 2
   1 > 4? NO

  j: 3
   4 > 4? NO
'''
def abacus_sort_2(a):
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
		k = 0
		for j in xrange(n):
			if a[j] > i:
				temp[k] += 1
				k += 1

	# Copy temp array back into original array
	# replacing the array into sorted order
	# temp array is reverse sorted, so copy backwards for ascending order
	for i in xrange(n):
		a[i] = temp[n-i-1]

	return a


# Move from maximum to minimum downwards
# simulating stacking up of the beads from the bottom-up
def abacus_sort_3(a):
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
	for i in xrange(maximum-1, minimum-1, -1):
		k = 0
		for j in xrange(n):
			if a[j] > i:
				temp[k] += 1
				k += 1

	# Copy temp array back into original array
	# replacing the array into sorted order
	# temp array is reverse sorted, so copy backwards for ascending order
	for i in xrange(n):
		a[i] = temp[n-i-1]

	return a


def test_abacus_sort(sortfn):
	a = [2,5,1,4]
	assert sortfn(a) == [1,2,4,5]

	b = [1, 2, 4, 2, 5, 3, 2, 3, 4]
	assert sortfn(b) == [1,2,2,2,3,3,4,4,5]

	# negative numbers
	c = [3, -1 , 2, -2, 4, 0, 1, 5, 3]
	assert sortfn(c) == [-2, -1, 0, 1, 2, 3, 3, 4, 5]

	assert sortfn([5, 3, 1, 7, 4, 1, 1, 20]) == [1, 1, 1, 3, 4, 5, 7, 20]

	assert sortfn([5, -1, 3, -2, 4, -3]) == [-3, -2, -1, 3, 4, 5]


if __name__ == '__main__':
	test_abacus_sort(abacus_sort)
	test_abacus_sort(abacus_sort_2)
	test_abacus_sort(abacus_sort_3)


