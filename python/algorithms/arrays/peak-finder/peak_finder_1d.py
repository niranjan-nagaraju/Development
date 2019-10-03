'''
Find /a/ peak in a 1-D array

a[i] is a peak if a[i-1] <= a[i], AND a[i] >= a[i+1]
'''

'''
Solution Outline:
	Brute force version: O(n)
	For each item, check if its a peak, return if it is.
'''

from is_peak import is_peak_1d
class PeakFinder(object):
	def __init__(self, array):
		self.a = array
		self.n = len(array)

	def find(self):
		for i in range(self.n):
			if is_peak_1d(self.a, i, self.n):
				return self.a[i], i


if __name__ == '__main__':
	assert PeakFinder([1,2,3,4]).find() == (4, 3)
	assert PeakFinder([4,3,2,1]).find() == (4, 0)
	assert PeakFinder([2,1,3,4]).find() == (2, 0)
	assert PeakFinder([1,3,2,4]).find() == (3, 1)
	assert PeakFinder([1,3,4,2]).find() == (4, 2)
	assert PeakFinder([1,1,2,3]).find() == (1, 0)

