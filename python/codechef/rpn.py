def isOperator (c):
	return (c == '+' or c == '-' or c == '*' or c == '/' or c == '%' or c == '^')
 
def convert_to_rpn (expression):
	stack = []
	i = 0
  
	while (i < len(expression)):
		if (expression[i] == '(' or isOperator(expression[i]) or expression[i].isalpha()):
			stack.append(expression[i])
		elif (expression[i] == ')'):
			right_operand, operator, left_operand = stack.pop(), stack.pop(), stack.pop()
			stack.pop() # discard the '('
			stack.append(left_operand + right_operand + operator)
		i = i + 1
	return stack.pop()

############
   
num_expressions = int (input())
    
expressions = []
while (num_expressions > 0):
	expression = raw_input()
	expressions.append(expression)
	num_expressions = num_expressions - 1
	 
rpn_expressions = []
for expression in expressions:
	rpn_expression = convert_to_rpn (expression)
	rpn_expressions.append(rpn_expression)
	  
for item in rpn_expressions:
	print item

