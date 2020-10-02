/**
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
*/

#include <cassert>
#include <sstream>
#include <vector>
#include <numeric>
#include <algorithm>

using VecOfStr = std::vector<std::string>;

class Solution {
public:
	std::string
	simplifyPath(std::string A) {
		if (!A.size())
			return A;

		VecOfStr stack;
		std::stringstream split_stream(A);
		while (split_stream.good()) {
			std::string dir;
			getline(split_stream, dir, '/');

			if (!dir.size() || dir == ".")
				continue;

			if (dir == "..") {
				// Go up one level
				if (stack.size()) {
					// but only if there are enough directories to go up by
					stack.pop_back();
				}
			} else {
				// a directory name
				stack.push_back(dir);
			}
		}

		// If we don;t have any directories in the stack,
		// return "/", 
		// Otherwise, stitch them together with '/' as separator
		return stack.size() ? std::accumulate(
						stack.begin(), stack.end(), std::string(""),
						[](std::string &a, std::string &b) {
							return a + "/" + b;
						}
					) : "/";

	}
};



int
main(void)
{
	Solution s;
	assert(s.simplifyPath("/home/") == "/home");
	assert(s.simplifyPath("/a/./b/../../c/") == "/c");
	assert(s.simplifyPath("/a/./b/../../c") == "/c");
	assert(s.simplifyPath("/a/./b/../../c/d/") == "/c/d");
	assert(s.simplifyPath("/home/../../..//") == "/");
	assert(s.simplifyPath("/home/../../../a/") == "/a"); 
	assert(s.simplifyPath("/home/../../../a///") == "/a"); 

	return 0;
}
