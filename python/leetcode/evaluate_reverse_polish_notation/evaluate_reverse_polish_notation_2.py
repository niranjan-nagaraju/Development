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
	In-place reduction
	Scan the input tokens list left->right, skipping operands.
	when an operator is encountered, pop operator and the last two items before it(right, left operands) and replace it
	  with the result (left-operand <operator> right-operand)


Sample run:
  tokens: ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
  i = 0

  0: "10"
  tokens: ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]

  1: "6"
  tokens: ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]

  2: "9"
  tokens: ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]

  3: "3"
  tokens: ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]

  4: "+" -> operator
    pop(4)
    tokens: ["10", "6", "9", "3", "-11", "*", "/", "*", "17", "+", "5", "+"]
	right = pop(3) : 3
    tokens: ["10", "6", "9", "-11", "*", "/", "*", "17", "+", "5", "+"]
	left = pop(2) : 9
    tokens: ["10", "6", "-11", "*", "/", "*", "17", "+", "5", "+"]
	value = 9+3 == 12
	insert(2, 12)
    tokens: ["10", "6", 12, "-11", "*", "/", "*", "17", "+", "5", "+"]
	i = 3


  3: "-11"	
    tokens: ["10", "6", 12, "-11", "*", "/", "*", "17", "+", "5", "+"]

  4: "*" -> operator
    pop(4)
	right = pop(3) : -11
	left = pop(2) : 12
    tokens: ["10", "6", "/", "*", "17", "+", "5", "+"]
	value = 12*-11 == -132
	insert(2, -132)
    tokens: ["10", "6", -132, "/", "*", "17", "+", "5", "+"]
	i = 3

  3: "/" -> operator
    pop(3)
	right = pop(2) : -132
	left = pop(1) : 6
    tokens: ["10", "*", "17", "+", "5", "+"]
	value = 6 / -132 == 0
	insert(1, 0)
    tokens: ["10", 1, "*", "17", "+", "5", "+"]
	i = 2

  2: "*" -> operator
    pop(2)
 	right = pop(1) : 0
	left = pop(0) : 10
    tokens: ["17", "+", "5", "+"]
	value = 10 * 0 == 0
	insert(0,0)
    tokens: [0, "17", "+", "5", "+"]
	i = 1

  1: "17"
    tokens: [0, "17", "+", "5", "+"]

  2: "+" -> operator
    pop(2)
 	right = pop(1) : 17
	left = pop(0) : 0
    tokens: ["5", "+"]
	value = 0 + 17 == 17
	insert(0,17)
    tokens: [17, "5", "+"]
	i = 1

  1: "5"
    tokens: [17, "5", "+"]

  "+" -> operator
    pop(2)
 	right = pop(1) : 5
	left = pop(0) : 17
    tokens: []
	value = 17 + 5 == 22
	insert(0, 22)
    tokens: [22]

Result: 17
'''

class Solution(object):
	def evalRPN(self, tokens):
		"""
		:type tokens: List[str]
		:rtype: int
		"""
		def evaluate(left, right, op):
			left, right = int(left), int(right)
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

		i = 0
		while len(tokens) > 1:
			x = tokens[i]
			if isOperator(x):
				tokens.pop(i) # remove operator
				right = tokens.pop(i-1)
				left = tokens.pop(i-2)
				value = evaluate(left, right, x)
				tokens.insert(i-2, value)
				# we have removed i-2, i-1, and i from the list, 
				# and added back the result into (i-2)
				# resume reduction from (i-2)+1 == i-1
				i = i-1 
			else: # operand -> skip ahead
				i += 1

		return tokens[0]


if __name__ == '__main__':
	s = Solution()
	a = ["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]
	assert s.evalRPN(a) == 22
	exit(0)
	assert s.evalRPN(["2", "1", "+", "3", "*"]) == 9
	assert s.evalRPN(["4", "13", "5", "/", "+"]) == 6
	assert s.evalRPN(["4", "13", "+", "5", "6", "*", "-"]) == -13

