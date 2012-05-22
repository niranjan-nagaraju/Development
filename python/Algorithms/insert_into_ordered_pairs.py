#!/usr/bin/python

# Amazon question:
# List of ordered pairs
# pairs stand for ranges - (lb, ub)
# Insert a new pair s.t. it remains ordered

def greater(a, b):
    if (a > b):
        return a
    else:
        return b

''' Merge two pairs if possible, Leave alone if disjoint ''' 
def merge_ordered_pairs(list_of_ordered_pairs, i, j):
    # (a,b) (c,d) : if c is between a and b, ergo <= d, then make it (a,greater(b,d))
    # (2,8) (3,10) => (2,10)
    # (2,6) (8,10) => (2,6) (8,10)
    # (2,10) (3,9) => (2,10)
    if (list_of_ordered_pairs[j][0]  <= list_of_ordered_pairs[i][1]):
        a = list_of_ordered_pairs[i][0]
        b = greater(list_of_ordered_pairs[i][1], list_of_ordered_pairs[j][1])
        list_of_ordered_pairs.pop(j)
        list_of_ordered_pairs.pop(i)
        list_of_ordered_pairs.insert(i, (a,b))


def insert_into_ordered_pairs(list_of_ordered_pairs, new_pair):
    i = 0
    while ( i < len(list_of_ordered_pairs) and new_pair[0] >= list_of_ordered_pairs[i][0] ):
        i = i + 1

    list_of_ordered_pairs.insert(i, new_pair) 

    merge_ordered_pairs(list_of_ordered_pairs, i-1, i)

    j = i
    while (j < len(list_of_ordered_pairs)-1 and (list_of_ordered_pairs[j][1] > list_of_ordered_pairs[j+1][1] or list_of_ordered_pairs[j][1] > list_of_ordered_pairs[j+1][0]) ):
        merge_ordered_pairs(list_of_ordered_pairs, j, j+1)
