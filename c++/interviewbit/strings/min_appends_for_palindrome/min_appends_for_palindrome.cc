/**
https://www.interviewbit.com/problems/minimum-appends-for-palindrome/

Minimum Appends for Palindrome!

Problem Description
Given a string A consisting of lowercase characters.
We need to tell minimum characters to be appended (insertion at end) to make the string A a palindrome.

Problem Constraints
1 <= |A| <= 105
A consists only of lower-case characters.

Input Format
First argument is an string A.

Output Format
Return a integer denoting the minimum characters to be appended (insertion at end) to make the string A a palindrome.

Example Input
Input 1:
 A = "abede"
Input 2:
 A = "aabb"

Example Output
Output 1:
 2
Output 2:
 2

Example Explanation
Explanation 1:
 We can make string palindrome as "abedeba" by adding ba at the end of the string.
Explanation 2:
 We can make string palindrome as "aabbaa" by adding aa at the end of the string.
*/



/**
Solution Outline:
	0. This is similar to the problem, 'Min characters to prepend for palindrome'
		https://www.interviewbit.com/problems/minimum-characters-required-to-make-a-string-palindromic/
		which is solved by appending A with A' (reversed A) with a '$' in between.
		A $ A'
		Calculate LPS(A$A'), (n-LPS[-1]) is the number of characters needed to prepend to A to make it a palindrome.
		e.g,
			A: BACB
			A': BCAB
			A$A': B A C B $ B C A B
			LPS: [0,0,0,1,0,1,0,0,1]
			min chars needed to prepend: n-1 = 3 => BCA needs to be prepended
				-> BCABACB to make BACB a palindrome

			A:  AACECAAAA
			A': AAAACECAA
			A$A': A A C E C A A A A $ A A A A C E C A A
			LPS: [0 1 0 0 0 1 2 2 2 0 1 2 2 2 3 4 5 6 7]
			min chars needed to prepend: n-7 = 9-7 = 2 => AA needs to be prepended
				-> AAAACECAAAA to make AACECAAAA a palindrome

	1. Calculate LPS(A'$A), (n-LPS[-1]) is the number of characters needed to append to A to make it a palindrome.
		e.g,
		A:  BACB
		A': BCAB
		A'$A: B C A B $ B A C B
		LPS: [0 0 0 1 0 1 0 0 1]
			min chars needed to append: n-1 = 3 => CAB needs to be appended
				-> BACBCAB to make BACB a palindrome
	
		e.g,
		A:  AACECAAAA
		A': AAAACECAA
		A'$A: A A A A C E C A A $ A A C E C A A A A
		LPS: [0 1 2 3 0 0 0 1 2 0 1 2 0 0 0 1 2 3 4]
			min chars to append: n-4 = 5 => CECAA needs to be appended
				-> AACECAAAACECAA to make AACECAAAA a palindrome
*/

#include <vector>
#include <string>
#include <iostream>
#include <assert.h>

class Solution
{
	public:
		/**
		 * Calculate LPS array from KMP-search
		 * for a given string
		 */
		std::vector<int> compute_lps_array(std::string A, int n)
		{
			std::vector<int> lps(n, 0);
			int i=1, lp=0;

			//lps[0] is 0
			while (i < n) {
				if (A[i] == A[lp]) {
					lps[i++] = ++lp;
				} else {
					/** Mismatch */
					if (lp == 0) {
						lps[i++] = 0;
					} else {
						/** 
						 * lp != 0
						 * Backtrack by lps[lp-1] to see if we can
						 * match past prefixes
						 */
						lp = lps[lp-1];
					}
				}
			}
			return lps;
		}


		/**
		 * Find minimum characters needed to append to A
		 * to make it a palindrome
		 */
		int min_chars_to_append(std::string A)
		{
			if (A.size() == 0)
				return 0;

			std::string A_(A.rbegin(), A.rend());
			auto lps = compute_lps_array(A_ + '$' + A, 2*A.size()+1);
			return A.size() - lps.back();
		}
};


int
main(void)
{
	Solution s;
	assert(s.min_chars_to_append("bacb") == 3);
	assert(s.min_chars_to_append("aacecaaaa") == 5);
	assert(s.min_chars_to_append("aaaacecaa") == 2);
	assert(s.min_chars_to_append("abede") == 2);
	assert(s.min_chars_to_append("aabb") == 2);
	assert(s.min_chars_to_append("aabc") == 3);

	return 0;
}

