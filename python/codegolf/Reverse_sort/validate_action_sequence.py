#!/usr/bin/python

# Reverse first n elements in the list
def reverse(inList, n):
	i = 0
	j = n - 1

	while i < j:
		tmp = inList[i]
		inList[i] = inList[j]
		inList[j] = tmp
		i = i+1; j = j-1

	return inList


def apply_action_sequence(actions_list, inlist):

	outlist = inlist[:]
	for i in actions_list:
		reverse(outlist, i)

	return outlist

inlist = input()

strList = raw_input()
strList = strList.split(' ')
actions_list = []

for i in strList:
	actions_list.append(int(i))

print apply_action_sequence(actions_list, inlist)

