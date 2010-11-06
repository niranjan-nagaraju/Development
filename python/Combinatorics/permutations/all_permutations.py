#!/usr/bin/python

def permute_all(a, n):
	perm_list = []
	for i in range(0, n):
		b = a[:]
		ele = b.pop(i)
		print "Ordering ", ele, b

		curr = ([ele] + b[:])
		if curr not in perm_list:
			perm_list.append(curr)
		print curr

		lst = range(1, n)
		print "range: ", lst
		for j in lst:		
			curr = (b[:j] + [ele] + b[j:])
			if curr not in perm_list:
				perm_list.append(curr)
			print curr
	
	return perm_list

lst = permute_all([1,2,3,4], 4)
lst.sort()
print
print

for i in lst:
	print i
