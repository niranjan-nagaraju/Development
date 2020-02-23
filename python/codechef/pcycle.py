
'''
http://www.codechef.com/problems/PCYCLE/
Permutation Cycles
'''

buckets = [0] * 1002

# recursive version has trouble handling 1000 levels
def perm_cycle_rec (inList, n, last_unvisited_pos, cycles):
	if last_unvisited_pos == n:
		return cycles

	x = inList[last_unvisited_pos]
	curr_cycle = [last_unvisited_pos]
	while x != last_unvisited_pos:
		curr_cycle.append(x) 
		x = inList[x]
	curr_cycle.append(x)

	cycles.append(curr_cycle)

	last_unvisited_pos = find_least_missing_number (curr_cycle)
	return perm_cycle_rec(inList, n, last_unvisited_pos, cycles)


def perm_cycle (inList, n):
	last_unvisited_pos = 1
	cycles = []

	while last_unvisited_pos != n:
		x = inList[last_unvisited_pos]
		curr_cycle = [last_unvisited_pos]
		while x != last_unvisited_pos:
			curr_cycle.append(x) 
			x = inList[x]
		curr_cycle.append(x)

		cycles.append(curr_cycle)

		last_unvisited_pos = find_least_missing_number (curr_cycle)
	
	return cycles


def find_least_missing_number (inList):
	for i in inList:
		buckets[i] = i

	for i in range(1, 1002):
		if buckets[i] != i:
			break
	
	return i

def perm_cycles (perm, n):
	# x = [0] + [2, 4, 5, 1, 7, 6, 3, 8]

	cycles = perm_cycle(perm, n)
	print len(cycles)
	for c in cycles:
		for i in c:
			print i,
		print

def getInput():
	n = int(input())
	perm = raw_input()
	perm = perm.strip()
	perm = [0] + map (int, (perm.split(' ')))

	return (n+1, perm)

(n, perm) = getInput()
#print n
#print perm
perm_cycles(perm, n)
