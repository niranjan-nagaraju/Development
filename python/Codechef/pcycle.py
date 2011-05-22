
'''
http://www.codechef.com/problems/PCYCLE/
Permutation Cycles
'''

def perm_cycle (inList, n):
	last_unvisited_pos = 1

	max_visited_pos = 1
	x = last_unvisited_pos + 1
	print last_unvisited_pos,
	while x != last_unvisited_pos:
		print x,
		x = inList[x]
	print x	

x = [0,2,4,5,1,7,6,3,8]
perm_cycle (x, 8)


