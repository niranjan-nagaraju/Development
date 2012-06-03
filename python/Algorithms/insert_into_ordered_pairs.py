#!/usr/bin/python

# Amazon question:
# List of ordered pairs
# pairs stand for ranges - (lb, ub)
# Insert a new pair s.t. it remains ordered

# Return greater of two numbers, ofcourse :)
def greater(a, b):
    if (a > b):
        return a
    else:
        return b

# Merge two pairs if possible, Leave alone if disjoint 
#    Return True if merge happened;
#		   False otherwise
def merge_ordered_pairs(list_of_ordered_pairs, i, j):

	''' A Little help from a friend who helps merge two pairs into one '''
	def merge_helper ():
		list_of_ordered_pairs.pop(j)
		list_of_ordered_pairs.pop(i)
		list_of_ordered_pairs.insert(i, (a,b))
	''' End merge helper '''

	a = list_of_ordered_pairs[i][0] # if it can be merged, a is always the new left in the pair
	b = 0

    # (a,b) (c,d) : if c is between a and b, then make it (a,greater(b,d))
    # (2,8) (3,10) => (2,10)
    # (2,6) (8,10) => (2,6) (8,10)
    # (2,10) (3,9) => (2,10)
	if (list_of_ordered_pairs[j][0]  <= list_of_ordered_pairs[i][1]):
		b = greater(list_of_ordered_pairs[i][1], list_of_ordered_pairs[j][1])
		merge_helper ()
		return True
	# if c is b+1, it's a continuous range, merge them into (a, d)
	# (2,6) (7,12) => (2,12) 
	elif (list_of_ordered_pairs[j][0] == list_of_ordered_pairs[i][1]+1):
		b = list_of_ordered_pairs[j][1]
		merge_helper ()
		return True

	return False


# Insert the new pair in the appropriate place first
# and keep merging from the left
# for as long as you can 
def insert_into_ordered_pairs(list_of_ordered_pairs, new_pair):
	i = 0
	while ( i < len(list_of_ordered_pairs) and new_pair[0] >= list_of_ordered_pairs[i][0] ):
		i = i + 1

	# Insert at the right place
	list_of_ordered_pairs.insert(i, new_pair) 

	# If the new pair wasn't inserted at the beginning, we are safe to begin merging from the left of it
	# else, we start right from the beginning
	if ( i != 0 ):
		i = i - 1

	# Keep merging as far as you possibly can
	while (i < len(list_of_ordered_pairs)-1 and 
	  merge_ordered_pairs(list_of_ordered_pairs, i, i+1) == True):
		continue

''' Trial runs

>>> l = [(2,6), (8,10), (14, 20)]; insert_into_ordered_pairs(l, (7, 18))
>>> l
[(2, 20)]


>>> l = [(2,6), (8,10), (14, 20)]; insert_into_ordered_pairs(l, (1, 18))
>>> l
[(1, 20)]


>>> l = [(2,6), (8,10), (14, 20)]; insert_into_ordered_pairs(l, (21, 28))
>>> l
[(2, 6), (8, 10), (14, 28)]


>>> l = [(2,6), (8,10), (14, 20)]; insert_into_ordered_pairs(l, (11, 12))
>>> l
[(2, 6), (8, 12), (14, 20)]

>>> l = [(2,6), (8,10), (18, 20)]; insert_into_ordered_pairs(l, (12, 14))
>>> l
[(2, 6), (8, 10), (12, 14), (18, 20)]

>>> l = [(2,6), (8,10), (18, 20)]; insert_into_ordered_pairs(l, (1, 4))
>>> l
[(1, 6), (8, 10), (18, 20)]

'''
