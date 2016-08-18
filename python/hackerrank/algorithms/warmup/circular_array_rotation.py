'''
https://www.hackerrank.com/challenges/circular-array-rotation

John Watson performs an operation called a right circular rotation on an array of integers, [a0, a1, .. a(n-1)]. After performing one right circular rotation operation, the array is transformed from [a0, a1, .. a(n-1)] to [a(n-1), a0, , .. a(n-2)].

Watson performs this operation k times. To test Sherlock's ability to identify the current element at a particular position in the rotated array, Watson asks q queries, where each query consists of a single integer, m, for which you must print the element at index m in the rotated array (i.e., the value of am).

Input Format
The first line contains space-separated integers, n, k, and q, respectively.
The second line contains n space-separated integers, where each integer i describes array element ai(where 0 <= i<= n).
Each of the subsequent q lines contains a single integer denoting m.

Output Format
For each query, print the value of the element at index of the rotated array on a new line.

Sample Input
3 2 3
1 2 3
0
1
2

Sample Output
2
3
1

'''

# Print what would be at a[m] after 'k' rotations
def answer_query(a, n, m, k):
	start_index = -k
	print a[(start_index+m)% n]


n, k, q = map(int, raw_input().strip().split())
a = map(int, raw_input().strip().split())
for i in xrange(q):
	m = int(raw_input())
	answer_query(a, n, m, k)

