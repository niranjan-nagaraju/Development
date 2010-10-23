#!/usr/bin/python

def reverse(list):
	i = len(list)/2 + 1
	for j in range(0, i):
		list[j], list[-j] = list[-j], list[j]
		print list

	return list

list = [1,2,3,4,5]
print reverse(list)
