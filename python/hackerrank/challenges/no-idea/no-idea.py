'''
https://www.hackerrank.com/challenges/no-idea/problem
'''
n,m = map(int, raw_input().strip().split())
arr = map(int, raw_input().strip().split())

# set is a hash-table internally and makes searches into it an O(1) operation
A = set()
B = set()

addToSetA = lambda x: A.add(int(x))
addToSetB = lambda x: B.add(int(x))

map(addToSetA, raw_input().strip().split())
map(addToSetB, raw_input().strip().split())

happiness = 0

for i in arr:
	if i in A:
		happiness += 1
	elif i in B:
		happiness -= 1

print happiness


'''
[16:53:08 no-idea]$ cat no-idea.in | python no-idea.py 
1
'''
