'''
https://www.interviewbit.com/problems/evaluate-expression/

Evaluate Expression


Evaluate the value of an arithmetic expression in Reverse Polish Notation.

Valid operators are +, -, *, /. Each operand may be an integer or another expression.


Input Format
The only argument given is character array A.

Output Format
Return the value of arithmetic expression formed using reverse Polish Notation.

For Example
Input 1:
    A =   ["2", "1", "+", "3", "*"]
Output 1:
    9
Explaination 1:
    starting from backside:
    *: ( )*( )
    3: ()*(3)
    +: ( () + () )*(3)
    1: ( () + (1) )*(3)
    2: ( (2) + (1) )*(3)
    ((2)+(1))*(3) = 9
    
Input 2:
    A = ["4", "13", "5", "/", "+"]
Output 2:
    6
Explaination 2:
    +: ()+()
    /: ()+(() / ())
    5: ()+(() / (5))
    1: ()+((13) / (5))
    4: (4)+((13) / (5))
    (4)+((13) / (5)) = 6
'''


'''
Solution Outline:
	1. Use a stack to reduce expressions as operators are encountered.
	2. The stack stores only operands, and 
		2.1 on encountering an operator,
		2.2 pop right followed by the left operand
		2.3 Apply operator on the operands, and push the reduced result onto the stack.
	3. For each operand, push it onto the stack.
'''

class Solution:
	def evaluate(self, expression):
		stack = []
		for x in expression:
			if x in ('+', '-', '*', '/'):
				try:
					right = stack.pop()
					left = stack.pop()

					# apply 'operator' on the right and left operands
					if x == '+':
						stack.append(left + right)
					elif x == '-':
						stack.append(left - right)
					elif x == '*':
						stack.append(left * right)
					elif x == '/':
						if right == 0:
							raise ValueError("Division by zero")
						stack.append(left / right)
					else: # unknown operator
						raise ValueError("Unknown operator")
				except IndexError:
					# Stack doesn't have enough operands
					# The expression is invalid
					raise ValueError("Invalid Expression")
			else:
				# operand
				stack.append(int(x))


		# Return top of the stack as the reduced expression value
		try:
			# If the epxression is valid,
			# there are no more than one  operands on the stack
			assert len(stack) == 1
			return stack.pop()
		except IndexError:
			raise ValueError("Invalid Expression")



if __name__ == '__main__':
	s = Solution()
	assert s.evaluate(["2", "1", "+", "3", "*"]) == 9
	assert s.evaluate(["4", "13", "5", "/", "+"]) == 6

