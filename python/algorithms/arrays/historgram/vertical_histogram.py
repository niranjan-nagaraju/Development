'''
Print a vertical histogram using * for a given set of numbers

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
import sys
def print_histogram(a):
	maximum = a[0]
	for x in a[1:]:
		if x > maximum:
			maximum = x

	for i in xrange(maximum):
		for j in xrange(len(a)):
			sys.stdout.write('* ' if a[j] > i else '  ')
		print



if __name__ == '__main__':
	print_histogram([2,6,1,4,3])
	'''
	* * * * * 
	* *   * * 
	  *   * * 
	  *   *   
	  *       
	  *       
	'''
