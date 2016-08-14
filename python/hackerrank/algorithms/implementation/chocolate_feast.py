'''
https://www.hackerrank.com/challenges/chocolate-feast

Little Bobby loves chocolate, and he frequently goes to his favorite 5&10 store, Penny Auntie, with n dollars to buy chocolates. Each chocolate has a flat cost of c dollars, and the store has a promotion where they allow you to trade in m chocolate wrappers in exchange for 1 free piece of chocolate.

For example, if m=2 and Bobby has n=4 dollars that he uses to buy 4 chocolates at c=1 dollar apiece, he can trade in the 4 wrappers to buy 2 more chocolates. Now he has 2 more wrappers that he can trade in for 1 more chocolate. Because he only has 1 wrapper left at this point and 1<m, he was only able to eat a total of 7 pieces of chocolate.

Given n, c, and m for t trips to the store, can you determine how many chocolates Bobby eats during each trip?

Input Format

The first line contains an integer, t, denoting the number of trips Bobby makes to the store.
Each line i of the subsequent t lines contains three space-separated integers describing the respective n, c, and m values for one of Bobby's trips to the store.

Constraints
1 <= t <= 1000
2 <= n <= 10**5
1 <= c <= n
2 <= m <= n

Output Format

For each trip to Penny Auntie, print the total number of chocolates Bobby eats on a new line.

Sample Input

3
10 2 5
12 4 4
6 2 2

Sample Output

6
3
5

Explanation

Bobby makes the following trips to the store:

    He spends his 10 dollars on 5 chocolates at 2 dollars apiece. He then eats them and exchanges all 5 wrappers to get 1 more chocolate. We print the total number of chocolates he ate, which is 6.
    He spends his 12 dollars on 3 chocolates at 4 dollars apiece; however, he needs 4 wrappers to trade for his next chocolate. Because he only has 3 wrappers, he cannot purchase or trade for any more chocolates. We print the total number of chocolates he ate, which is 3.
    He spends 6 dollars on 3 chocolates at 2 dollars apiece. He then 2 exchanges of the 3 wrappers for 1 additional piece of chocolate. Next, he uses his third leftover chocolate wrapper from his initial purchase with the wrapper from his trade-in to do a second trade-in for 1 more piece of chocolate. At this point he has 1 wrapper left, which is not enough to perform another trade-in. We print the total number of chocolates he ate, which is 5.

'''

def chocolates_to_eat(n, c, m):
	chocolates = n/c
	wrappers = chocolates

	while (wrappers >= m):
		# chocolates from this trip 
		chocolates_t = (wrappers / m)

		# total chocolates
		chocolates += chocolates_t

		# wrappers from current chocolates +  remaining from previous transaction
		wrappers = chocolates_t + (wrappers % m) 

	return chocolates

#print chocolates_to_eat(10, 2, 5) -> 6
#print chocolates_to_eat(12, 4, 4) -> 3
#print chocolates_to_eat(6, 2, 2) -> 5

t = int(raw_input().strip())
for a0 in xrange(t):
	n,c,m = map(int, raw_input().strip().split(' '))
	print chocolates_to_eat(n, c, m)


