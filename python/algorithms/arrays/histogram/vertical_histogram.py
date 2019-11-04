import sys

'''
Print an upside-down vertical histogram using * for a given set of numbers

e.g.,

a = [2, 6, 1, 4, 3]

Histogram:
    * * * * *
    * *   * *
      *   * *
      *   *
      *
      *
'''
def print_histogram_upside_down(a):
	maximum = a[0]
	for x in a[1:]:
		if x > maximum:
			maximum = x

	for i in xrange(maximum):
		for j in xrange(len(a)):
			sys.stdout.write('* ' if a[j] > i else '  ')
		print

'''
Print a vertical histogram using * for a given set of numbers

e.g.,

a = [2, 6, 1, 4, 3]

Histogram:

      *
      *
      *   *
      *   * *
	* *   * *
    * * * * *
'''
def print_histogram(a):
	maximum = a[0]
	for x in a[1:]:
		if x > maximum:
			maximum = x

	for i in xrange(maximum-1, -1, -1):
		for j in xrange(len(a)):
			sys.stdout.write('* ' if a[j] > i else '  ')
		print




if __name__ == '__main__':
	print_histogram_upside_down([2,6,1,4,3])
	'''
	* * * * * 
	* *   * * 
	  *   * * 
	  *   *   
	  *       
	  *       
	'''
	print_histogram([2,6,1,4,3])
	'''
      *       
      *       
      *   *   
      *   * * 
    * *   * * 
    * * * * * 
	'''
