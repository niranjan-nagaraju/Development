'''
https://www.interviewbit.com/problems/balanced-parantheses/

Balanced Parantheses!

Problem Description

Given a string A consisting only of '(' and ')'.
You need to find whether parantheses in A is balanced or not ,if it is balanced then return 1 else return 0.


Problem Constraints
1 <= |A| <= 105


Input Format
First argument is an string A.

Output Format
Return 1 if parantheses in string are balanced else return 0.

Example Input
Input 1:
 A = "(()())"
Input 2:
 A = "(()"

Example Output
Output 1:
 1
Output 2:
 0

Example Explanation
Explanation 1:
 Given string is balanced so we return 1
Explanation 2:
 Given string is not balanced so we return 0
'''

'''
Solution Outline:
	0. Use a stack
	1. For each x in A
		1.1 If x is '(' -> push onto the stack
		1.2 otherwise, x is ')' -> pop a matching '(' off the stack
			  If pop() fails due to stack-underflow, => unbalanced; return 0
	2. At the end of the pass, if the stack isn't empty => there was a surplus of '('
		=> unbalanced, return 0
		2.1 If the stack is empty at the end of the pass, parantheses are balanced -> return 1
'''
class Solution:
	# @param A : string
	# @return an integer
	def solve(self, A):
		stack = []
		for x in A:
			try:
				if x == '(':
					stack.append(x)
				else:
					stack.pop()
			except IndexError:
				return 0

		if stack:
			return 0

		return 1


if __name__ == '__main__':
	s = Solution()
	assert s.solve('(())') == 1
	assert s.solve('())') == 0
	assert s.solve('(()') == 0
	assert s.solve('())(') == 0
	assert s.solve('(()())') == 1
