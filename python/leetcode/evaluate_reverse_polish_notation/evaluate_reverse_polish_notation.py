'''
https://leetcode.com/problems/evaluate-reverse-polish-notation/

150. Evaluate Reverse Polish Notation


Evaluate the value of an arithmetic expression in Reverse Polish Notation.

Valid operators are +, -, *, /. Each operand may be an integer or another expression.

Note:
Division between two integers should truncate toward zero.
The given RPN expression is always valid. That means the expression would always evaluate to a result and there won't be any divide by zero operation.

Example 1:
Input: ["2", "1", "+", "3", "*"]
Output: 9
Explanation: ((2 + 1) * 3) = 9

Example 2:
Input: ["4", "13", "5", "/", "+"]
Output: 6
Explanation: (4 + (13 / 5)) = 6

Example 3:
Input: ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
Output: 22
Explanation: 
  ((10 * (6 / ((9 + 3) * -11))) + 17) + 5
= ((10 * (6 / (12 * -11))) + 17) + 5
= ((10 * (6 / -132)) + 17) + 5
= ((10 * 0) + 17) + 5
= (0 + 17) + 5
= 17 + 5
= 22
'''


'''
Solution Outline:
	Use a stack.
	Scan the input tokens list left->right, pushing operands onto the stack
	when an operator is encountered, pop last two items off the stack(right, left operands) and replace it
	  with the result (left-operand <operator> right-operand)


Sample run:
  tokens: ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
  stack: []

  "10"
    stack: [10]

  "6"
    stack: [10, 6]

  "9"
    stack: [10, 6, 9]

  "3"
    stack: [10, 6, 9, 3]

  "+" -> operator
	right = pop() : 3
	left = pop() : 9
	value = 9+3 == 12
	push(12)
    stack: [10, 6, 12]

  "-11"	
    stack: [10, 6, 12, -11]

  "*" -> operator
	right = pop() : -11
	left = pop() : 12
	value = 12*-11 == -132
	push(-132)
    stack: [10, 6, -132]

  "/" -> operator
	right = pop() : -132
	left = pop() : 6
	value = 6 / -132 == 0
	push(0)
    stack: [10, 0]

  "*" -> operator
 	right = pop() : 0
	left = pop() : 10
	value = 10 * 0 == 0
	push(0)
    stack: [0]

  "17"
    stack: [0, 17]

  "+" -> operator
 	right = pop() : 17
	left = pop() : 0
	value = 0 + 17 == 17
	push(17)
    stack: [17]

  "5"
    stack: [17, 5]

  "+" -> operator
 	right = pop() : 5
	left = pop() : 17
	value = 17 + 5 == 22
	push(22)
    stack: [17]

Result: 17
'''

class Solution(object):
	def evalRPN(self, tokens):
		"""
		:type tokens: List[str]
		:rtype: int
		"""
		def evaluate(left, right, op):
			if op == '+':
				return left + right
			elif op == '-':
				return left - right
			elif op == '*':
				return left * right
			elif op == '/':
				# python2 / is weird
				# 1/-10 returns -1 instead of 0 (floor division)
				return int(float(left) / right)
			else:
				# ???
				return 0

		isOperator = lambda token: True if token in ('+', '-', '*', '/') else False
		stack = []

		for x in tokens:
			if isOperator(x):
				right = stack.pop()
				left = stack.pop()
				value = evaluate(left, right, x)
				stack.append(value)
			else:
				operand = int(x)
				stack.append(operand)

		return stack[0]


if __name__ == '__main__':
	s = Solution()
	a = ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
	assert s.evalRPN(a) == 22
	assert s.evalRPN(["2", "1", "+", "3", "*"]) == 9
	assert s.evalRPN(["4", "13", "5", "/", "+"]) == 6
	assert s.evalRPN(["4", "13", "+", "5", "6", "*", "-"]) == -13

