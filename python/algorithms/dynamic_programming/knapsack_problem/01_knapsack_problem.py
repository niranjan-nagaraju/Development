'''
0/1 knapsack problem:
	Given 'n' items of known weights w1, w2, ..., wn and values v1, v2, ..., vn and 
	a knapsack of capacity 'W', find the most valuable subset of the items that fit into the knapsack.
'''


''' 
Solution:
	Consider V[i][j] as a solution involving 'i' items (w1,..,wi),(v1,...,vi) with knapsack capacity 'j'.
	V[0][X] = 0 and V[X][0] = 0
	Starting with V[1], Item 1(w1, v1)... Include/exclude based on capacity 'j',
	With V[2] considering Items 1 and 2, but we have already solved for item 1, so V[2] essentially becomes 
	a decision to include Item 2 or not.
	. . .
	Repeat until V[n][W]
	V[n][W] is the largest value possible.

	To get the items needed, Start with V[n][W] and work backwards.
	  If V[n-1][W] == V[n][W] => we skipped Item 'n', otherwise we included it.
	  Deduct w[n] if we included it or don't and switch to v[n-1][<remaining weight>]
	  until <remaining weight == 0> *OR* we reach V[0]
'''	
def fill_knapsack(n, w, v, W):
	V = [ [0]*(W+1) for x in range(n+1) ]

	for i in range(1, n+1):
		for j in range(1, W+1):
			if (j >= w[i]): # knapsack capacity 'j' considered is big enough for w[i]
				V[i][j] = max ( V[i-1][j], # Excluding Item{i}
								(v[i] + V[i-1][j-w[i]]) # Including item{i}
							  )
			else: # j < w[i]
				V[i][j] = V[i-1][j] # knapsack capacity 'j' considered < w[i], Can't include this

	return V

	
def knapsack_items_needed(V, n, W):
	if W == 0:
		return []

	if (V[n][W] == V[n-1][W]): # Item 'n' excluded
		return knapsack_items_needed(V, n-1, W)
	else: # Item 'n' included
		return [n] + knapsack_items_needed(V, n-1, W-w[n])

n, W = 4, 5
w = [0,2,1,3,2]
v = [0,12,10,20,15]

V = fill_knapsack(n, w, v, W)
for i in range(n+1):
	for j in range(W+1):
		print V[i][j], "\t",
	print "\n"

items = knapsack_items_needed(V, n, W)
print 'Items needed: ' 
for i in items:
	print i, '| w:', w[i], 'v:', v[i]


'''
TC: 1
n, W = 4, 5
w = [0,2,1,3,2]
v = [0,12,10,20,15]
[00:23:44 knapsack_problem]$ python 01_knapsack_problem.py 
0       0       0       0       0       0 

0       0       12      12      12      12 

0       10      12      22      22      22 

0       10      12      22      30      32 

0       10      15      25      30      37 

Items needed: 
4 | w: 2 v: 15
2 | w: 1 v: 10
1 | w: 2 v: 12
'''

