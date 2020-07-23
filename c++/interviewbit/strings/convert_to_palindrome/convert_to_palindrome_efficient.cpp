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


/**
Solution Outline: O(n)
	0. If A is already a palindrome, removing the middle element will still render A palindromic.
		e.g, 
			Case I:  A is a palindrome of odd-length
				A: "racecar"
					removing "e" from A
					A': raccar -> still a palindrome
			Case II: A is a palindrome of even-length
				A: "redder"
					removing "d" from A
					A': reder -> still a palindrome
	1. The only time, removing a single character from A will not make it a palindrome is when
		  X X X a . . . e X X X
		  the left and right parts match and somewhere in the middle, we have two non-matching characters.
	2. At this point we can check if {a . . .} or {. . . e} is a palindrome
		If either of them are, then it means removing either a or e will render A a palindrome.
		If neither are, then we need to remove atleast 2 characters to make A a palindrome.


Sample run 1:
	A: "abcbceba"

	match left and right parts
	"a" vs "a"
	"ab" vs "ba"

	first mismatch is at 'c' and 'e' => substring: 'cbce'
	check if 'bce' is a palindrome - NO
	check if 'cbc' is a palindrome - YES
	removing 'e' from A will make it a palindrome
	A': "abcbcba"

Sample run 2:
	A: "abecbea"

	match left and right parts
	"a" vs "a"

	first mismatch at 'b' vs 'e' => substring: "becbe"
	check if 'ecbe' is a palindrome - NO
	check if 'becb' is a palindrome - NO
	A cannot be made a palindrome by removing exactly 1 character.


Sample run 3:
	A: "racecar"

	match left and right parts
	"r" vs "r"
	"ra" vs "ar"
	"rac" vs "car"
	"race" vs "ecar"

	no mismatch in left and right substrings
	=> A is already a palindrome
	removing 1 character will leave A a palindrome
	A': "raccar"
	Chipping 1 middle character from a palindrome until the end will keep yielding a palindrome
	"raccar" -> "racar" -> "raar" -> "rar" -> "rr" -> "r" -> ""
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

			int i = 0, j = A.size()-1;

			while (i < j and A[i] == A[j]) {
				i++;
				j--;
			}

			/** A is a palindrome */
			if (i >= j)
				return 1;

			// First mismatch at A[i] vs A[j]
			if (is_palindrome(A, i, j-1) or is_palindrome(A, i+1, j))
				return 1;

			return 0;
		}


		/*
		 * Check if A[i..j]
		 * is a palindrome
		 */
		bool
		is_palindrome(std::string A, int i, int j)
		{
			while (i < j) {
				if (A[i] != A[j])
					return false;

				i++;
				j--;
			}
			return true;
		}
};



int
main(void)
{
	Solution s;

	assert(s.can_convert_to_palindrome("racecar") == 1);
	assert(s.can_convert_to_palindrome("redder") == 1);
	assert(s.can_convert_to_palindrome("abceba") == 1);
	assert(s.can_convert_to_palindrome("abcdeba") == 0);
	assert(s.can_convert_to_palindrome("abecbea") == 0);
	assert(s.can_convert_to_palindrome("abcbea") == 1);

	assert(s.can_convert_to_palindrome("raccar") == 1);
	assert(s.can_convert_to_palindrome("racar") == 1);
	assert(s.can_convert_to_palindrome("raar") == 1);
	assert(s.can_convert_to_palindrome("rar") == 1);
	assert(s.can_convert_to_palindrome("rr") == 1);
	assert(s.can_convert_to_palindrome("r") == 1);
	assert(s.can_convert_to_palindrome("") == 0);

	return 0;
}
