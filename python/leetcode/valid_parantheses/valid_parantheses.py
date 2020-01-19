'''
https://leetcode.com/problems/valid-parentheses/

20. Valid Parentheses

Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

An input string is valid if:
Open brackets must be closed by the same type of brackets.
Open brackets must be closed in the correct order.
Note that an empty string is also considered valid.

Example 1:
Input: "()"
Output: true

Example 2:
Input: "()[]{}"
Output: true

Example 3:
Input: "(]"
Output: false

Example 4:
Input: "([)]"
Output: false

Example 5:
Input: "{[]}"
Output: true
'''

'''
Solution Outline:
  Use a stack
  push all opening brackets
  when a closing bracket of any type is encountered,
	pop() if the matching opening bracket of same type is seen on the stack top.
  If stack isn't empty at the end of the string, or if pop() fails anytime due to underflow => parantheses are invalid.
  Otherwise, they are valid.


Solution outline:
	s: "{[]}"
	stack: []
	
	'{':
	   stack: '{'
	
	 '[':
	   stack: '[', '{'

	 ']':
	   pop stack if top == '['
	   stack: '{'

	 '}':
	   pop stack if top == '{'
	   stack: []
'''

class Solution(object):
	def isValid(self, s):
		"""
		:type s: str
		:rtype: bool
		"""
		def matching_pairs(p):
			if p == ')':
				return '('
			elif p == ']':
				return '['
			else:
				return '{'

		stack = []

		for p in s:
			if p in ('(', '[', '{'):
				stack.append(p)
			else:
				if not stack or stack[-1] != matching_pairs(p):
					return False
				stack.pop()

		return True if not stack else False


if __name__ == '__main__':
	s = Solution()
	assert s.isValid("") == True
	assert s.isValid("()[]") == True
	assert s.isValid("()[]{}") == True
	assert s.isValid("(]") == False
	assert s.isValid("[])") == False
	assert s.isValid("([)]") == False
	assert s.isValid("{[]}") == True
	assert s.isValid(")") == False


