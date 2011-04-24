#!/usr/bin/python

# TODO: Cycle finding logic needs to be revised.
# Answer: 983, but the cycle length is 884 NOT 982 :(

# Get the list of quotients when 1 divides b
def quotients (a, b, quots, rems):
	i = 0
	while ( a < b ):
		a = a * 10
		i = i + 1

	# We don't need the 0's in the quotient. we are counting cycle length anyways
	# NOTE: 
	#	what about a quotient such as ..104104.. 
	#	Lets hope that never happens :)
	for i in range(1, i):
		quots.append(0)

	if (a%b in rems):
		quots.append(a/b) 
		return quots
	
	quots.append(a/b)

	if (a%b == 0):
		return quots

	rems.append(a%b)
	quotients (a%b, b, quots, rems)

	return quots


# The distance between the last element and its duplicate from the left gives the cycle length
# if the last element does not recur, there never was a cycle
def cycle_length (quots):
	idx = quots.index(quots[-1])

	if idx == len(quots) - 1:
		return 0

	return (len(quots) - idx - 1)
	

(max, ele) = (0, 0)
for i in range(1, 1000):
	quots = quotients(1, i, [],[])
	print i, quots,
	curr = cycle_length(quots)
	print curr
	if  curr > max:
		(max, ele) = (curr, i)

print 'Number: {0}, Cycle length: {1}'.format(ele, max)
