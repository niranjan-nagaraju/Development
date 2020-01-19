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
Solution Outline: O(1) memory, O(n) time
	There are only 3 types of parantheses
	Use 3 different counters for each type.
	  flower, square, round
	Add +1 to appropriate counter on opening types, -1 on closing types.
	If at any time, the counters go -ve, parantheses are invalid.
	At the end of the string, all counters are 0 if the parantheses were properly matched.


NOTE: FIXME
	False positives:
	   ([)] will be considered valid by this logic.

Solution outline:
	s: "{[]}()"
	flower, square, round = (0,0,0)
	
	'{':
	   flower, square, round = (1,0,0)
	
	 '[':
	   flower, square, round = (1,1,0)

	 ']':
	   flower, square, round = (1,0,0)

	 '}':
	   flower, square, round = (0,0,0)

	 '(':
	   flower, square, round = (0,0,1)

	 ')':
	   flower, square, round = (0,0,0)

	VALID
'''

class Solution(object):
	def isValid(self, s):
		"""
		:type s: str
		:rtype: bool
		"""

		# returns 0, 1, 2 for flower types, brackets and round types respectively
		def paren_type(p):
			if p in '{}':
				return 0
			elif p in '[]':
				return 1
			else: # ( or )
				return 2

		counters = [0, 0, 0]
		for p in s:
			ptype = paren_type(p)
			if p in ('(', '[', '{'): # opening brackets
				counters[ptype] += 1
			else: # closing brackets
				counters[ptype] -= 1
				if counters[ptype] < 0:
					return False
			
		return True if counters == [0,0,0] else False
			



if __name__ == '__main__':
	s = Solution()
	assert s.isValid("") == True
	assert s.isValid("()[]") == True
	assert s.isValid("()[]{}") == True
	assert s.isValid("(]") == False
	assert s.isValid("[])") == False
	assert s.isValid("([)]") == False  # FAILS
	assert s.isValid("{[]}") == True
	assert s.isValid(")") == False


