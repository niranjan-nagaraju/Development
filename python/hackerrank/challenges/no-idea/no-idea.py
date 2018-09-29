'''
https://www.hackerrank.com/challenges/no-idea/problem

A, B are two _disjoint_ sets with lengths n, and m respectively
for every element x in input array 'arr', 
   if x is in A, add 1 to happiness score
   if x is in B, subtract 1 from happiness score

A and B are disjoint sets means A and B dont have any common elements between them
A and B are sets => there are no duplicate elements within A or B
'''
n,m = map(int, raw_input().strip().split())
arr = map(int, raw_input().strip().split())

# set is a hash-table internally and makes searches into it an O(1) operation
A = set()
B = set()

# Use closure functions to implement 'partial lambda functions' a la haskell
def addToSet(in_set):
	# return inner closed function that has 'set to be inserted into' closed
	return lambda x: in_set.add(int(x))


map(addToSet(A), raw_input().strip().split())
map(addToSet(B), raw_input().strip().split())

happiness = 0

for i in arr:
	if i in A:
		happiness += 1
	elif i in B:
		happiness -= 1

print happiness


'''
[16:53:05 no-idea]$ cat no-idea.in
3 2
1 5 3
3 1
5 7

[16:53:08 no-idea]$ cat no-idea.in | python no-idea.py 
1
'''
