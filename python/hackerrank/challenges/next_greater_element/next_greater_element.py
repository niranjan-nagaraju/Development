'''
https://www.hackerrank.com/contests/second/challenges/next-greater-element

Given an array, print the Next Greater Element (NGE) for every element. The Next greater Element for an element x is the first greater element on the right side of x in array. Elements for which no greater element exist, consider next greater element as -1.

For the input array [4, 5, 2, 25}, the next greater elements for each element are as follows.

Element --> NGE
4 --> 5
5 --> 25
2 --> 25
25 --> -1

For the input array [13, 7, 6, 12}, the next greater elements for each element are as follows.

Element --> NGE
13 --> -1
7 --> 12
6 --> 12
12 --> -1


Input Format
The first line of input contains an integer n denoting the size of the array
The next line contains n space seperated array elements in integer range
0 < n < = 65535

Output Format
Output consists of n lines
Each line should contain 2 space seperated integers
The first integer should represent the array element and second integer should represent the next greater element

Sample Input
4
4 5 2 25

Sample Output
4 5
5 25
2 25
25 -1
'''


'''
Solution outline
    0. Initialize nge = [-1]*n
		nge : [-1, -1, -1, ..., -1]
	1. Use a stack and solve the problem of next-greater-element like matching parantheses.
	2. For each item in array, array[i], pop every x from the stack if array[i] > array[x]
		also record nge[x] = array[i]
	3. Push i onto stack	
'''
def next_greater_element(array):
	stack = []
	nge = [-1] * len(array)

	for i in xrange(len(array)):
		while stack and array[stack[0]] < array[i]:
			x = stack.pop(0)
			nge[x] = array[i]
		stack.insert(0, i)

	return nge


if __name__ == '__main__':
	assert next_greater_element([1,2,3,4]) == [2,3,4,-1]
	assert next_greater_element([3,1,2,4]) == [4,2,4,-1]
	assert next_greater_element([4,5,2,25]) == [5,25,25,-1]
	assert next_greater_element([13,7,6,12]) == [-1,12,12,-1]
	assert next_greater_element([4,3,2,1]) == [-1,-1,-1,-1]

