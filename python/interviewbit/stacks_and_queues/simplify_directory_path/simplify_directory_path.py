'''
https://www.interviewbit.com/problems/simplify-directory-path/

Simplify Directory Path

Given a string A representing an absolute path for a file (Unix-style).

Return the string A after simplifying the absolute path.

Note:
	Absolute path always begin with '/' ( root directory ).
	Path will not have whitespace characters.


Input Format
	The only argument given is string A.

Output Format
	Return a string denoting the simplified absolue path for a file (Unix-style).
	
For Example
Input 1:
A = "/home/"

Output 1:
"/home"
			
Input 2:
A = "/a/./b/../../c/"

Output 2:
"/c"
'''
class Solution:
	def simplifyPath(self, A):
		if not A:
			return A

		stack = []
		for x in A.split('/'):
			if not x or x == '.':
				# no-op
				continue

			if x == '..':
				# Go up one level
				if stack:
					# but only if there are enough directories to go up by
					stack.pop()
			else:
				# a directory name
				stack.append(x)

		# If we don;t have any directories in the stack,
		# return "/", 
		# Otherwise, stitch them together with '/' as separator
		return '/'+'/'.join(stack)



if __name__ == '__main__':
	s = Solution()
	assert s.simplifyPath("/home/") == "/home" 
	assert s.simplifyPath("/a/./b/../../c/") == "/c"
	assert s.simplifyPath("/a/./b/../../c") == "/c"
	assert s.simplifyPath("/a/./b/../../c/d/") == "/c/d"
	assert s.simplifyPath("/home/../../..//") == "/"
	assert s.simplifyPath("/home/../../../a/") == "/a" 
	assert s.simplifyPath("/home/../../../a///") == "/a" 

