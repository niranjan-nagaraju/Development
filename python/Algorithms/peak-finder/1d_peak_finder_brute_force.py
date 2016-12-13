'''
Find /a/ peak in a 1-D array

a[i] is a peak if a[i-1] <= a[i], AND a[i] >= a[i+1]
'''

from is_peak import is_peak_1d

def find_peak(a, n):
	for i in range(n):
		if is_peak_1d(a, i, n):
			return a[i], i

a = map(int, raw_input().strip().split())
peak, idx = find_peak(a, len(a))
print 'Peak {0} is at index {1}'.format(peak, idx)


'''
Testcases:

[14:18:45 peak-finder]$ python 1d_peak_finder_brute_force.py 
1 2 3 4
Peak 4 is at index 3

[14:18:59 peak-finder]$ python 1d_peak_finder_brute_force.py 
4 3 2 1
Peak 4 is at index 0

[14:19:03 peak-finder]$ python 1d_peak_finder_brute_force.py 
2 1 3 4
Peak 2 is at index 0

[14:19:14 peak-finder]$ python 1d_peak_finder_brute_force.py 
1 3 2 4
Peak 3 is at index 1

[14:19:43 peak-finder]$ python 1d_peak_finder_brute_force.py 
1 3 4 2
Peak 4 is at index 2

[14:20:03 peak-finder]$ python 1d_peak_finder_brute_force.py 
1 1 2 3
Peak 1 is at index 0

'''
