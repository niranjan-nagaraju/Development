'''
https://www.hackerrank.com/challenges/kangaroo

There are two kangaroos on an x-axis ready to jump in the positive direction (i.e, toward positive infinity). The first kangaroo starts at location x1 and moves at a rate of v1 meters per jump. The second kangaroo starts at location x2 and moves at a rate of v2 meters per jump. Given the starting locations and movement rates for each kangaroo, can you determine if they'll ever land at the same location at the same time?

Input Format

A single line of four space-separated integers denoting the respective values of x1, v1, x2, and v2.

Constraints
0 <= x1 < x2 <= 10000
1 <= v1,v2 <= 10000

Output Format
Print YES if they can land on the same location at the same time; otherwise, print NO.

Note: The two kangaroos must land at the same location after making the same number of jumps.

Sample Input 0
0 3 4 2

Sample Output 0
YES

Sample Input 1
0 2 5 3

Sample Output 1
NO

'''



'''
Let kangaroos meet after 't' jumps

=> x1 + v1*t = x2 + v2*t
=> x1 - x2 = v2*t - v1*t
=> t*(v1-v2) = (x2-x1)
=> t = (x2-x1) / (v1-v2)

if 't' is not an integer, => NO, otherwise YES

=> (x2-x1) % (v1-v2) must be 0

But if v2 > v1, then they'll never catch up 
'''


x1,v1,x2,v2 = map(int, raw_input().strip().split(' '))
if (v2 > v1) or (v2 == v1):
	# x2 > x1 => if v1 == v2, they'll never meet, also % by 0 would be a runtime error.
	print "NO"
	exit(0)

# print "YES" if ((x2-x1) % (v1-v2) == 0) else "NO"
if ((x2-x1) % (v1-v2) == 0):
	print "YES"
	#print 'They meet after', ((x2-x1) / (v1-v2)) , 'hops'
else:
	print "NO"
