'''
https://www.hackerrank.com/challenges/plus-minus

Print a fraction of positives, negatives and zeroes in list

Sample Input
6
-4 3 -9 0 4 1         

Sample Output
0.500000
0.333333
0.166667

'''
#!/bin/python

import sys


n = int(raw_input().strip())
arr = map(int,raw_input().strip().split(' '))

positives = 0.0
negatives = 0.0
zeroes = 0.0
for x in arr:
    if x > 0:
        positives += 1
    elif x < 0:
        negatives += 1
    else:
        zeroes += 1
        
print "%0.6f" % (positives/n)
print "%0.6f" % (negatives/n)
print "%0.6f" % (zeroes/n)
