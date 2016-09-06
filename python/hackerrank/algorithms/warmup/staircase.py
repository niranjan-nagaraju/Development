'''
https://www.hackerrank.com/challenges/staircase

Sample Input
6 

Sample Output
     #
    ##
   ###
  ####
 #####
######

'''

#!/bin/python

import sys


n = int(raw_input().strip())

for i in xrange(1, n+1):
	sys.stdout.write(' ' * (n-i))
	sys.stdout.write('#' * i)
	print

