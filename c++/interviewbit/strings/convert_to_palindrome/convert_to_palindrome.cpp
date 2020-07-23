/**
https://www.interviewbit.com/problems/convert-to-palindrome/

Convert to Palindrome

Problem Description
Given a string A consisting only of lowercase characters, we need to check whether it is possible to make this string a palindrome after removing exactly one character from this.
If it is possible then return 1 else return 0.


Problem Constraints
3 <= |A| <= 105
A[i] is always a lowercase character.

Input Format
First and only argument is an string A.

Output Format
Return 1 if it is possible to convert A to palindrome by removing exactly one character else return 0.


Example Input
Input 1:
 A = "abcba"
Input 2:
 A = "abecbea"


Example Output
Output 1:
 1
Output 2:
 0


Example Explanation
Explanation 1:
 We can remove character ‘c’ to make string palindrome
Explanation 2:
 It is not possible to make this string palindrome just by removing one character
*/

#include <iostream>
#include <assert.h>


class Solution
{
	public:
		int
		can_convert_to_palindrome(std::string A)
		{
			if (A.size() == 0)
				return 0;

			/** A is already a palindrome */
			if (is_palindrome(A, -1))
				return 1;

			for (auto i=0; i<A.size(); i++) {
				if (is_palindrome(A, i))
					return 1;
			}

			return 0;
		}


		/*
		 * Check if A[0:skip_index-1, skip_index+1..n-1]
		 * is a palindrome
		 */
		bool
		is_palindrome(std::string A, int skip_index)
		{
			int i = 0, j = A.size()-1;

			while (i < j) {
				if (i == skip_index) {
					i++;
				} else if (j == skip_index) {
					j--;
				} else {
					if (A[i] != A[j])
						return false;

					i++;
					j--;
				}
			}
			return true;
		}
};



int
main(void)
{
	Solution s;
	assert(s.is_palindrome("abceba",2) == true);
	assert(s.is_palindrome("abceba", 3) == true);
	assert(s.is_palindrome("abceba", 0) == false);
	assert(s.is_palindrome("abceba", 5) == false);
	assert(s.is_palindrome("abceba", 100) == false); // nothing to skip

	assert(s.can_convert_to_palindrome("racecar") == 1);
	assert(s.can_convert_to_palindrome("redder") == 1);
	assert(s.can_convert_to_palindrome("abceba") == 1);
	assert(s.can_convert_to_palindrome("abcdeba") == 0);
	assert(s.can_convert_to_palindrome("abecbea") == 0);
	assert(s.can_convert_to_palindrome("abcbea") == 1);
	
	return 0;
}
