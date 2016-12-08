'''
https://www.hackerrank.com/challenges/connecting-town

Sample Input
2
3
1 3
4
2 2 2

Sample Output
3
8

Explanation
Case 1: 1 route from T1 to T2, 3 routes from T2 to T3, hence only 3 routes.
Case 2: There are 2 routes from each city to the next, at each city, Gandalf has 2 choices to make, hence 2 * 2 * 2 = 8.

'''

T = int(input()) # Testcases
for i in range(T):
    n = int(input())
    ni = map(int, raw_input().split())
    print reduce(lambda x, y: x*y, ni)
