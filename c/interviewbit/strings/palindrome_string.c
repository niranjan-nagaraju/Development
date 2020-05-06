/**
https://www.interviewbit.com/problems/palindrome-string/

Given a string, determine if it is a palindrome, considering only alphanumeric characters and ignoring cases.

Example:
	"A man, a plan, a canal: Panama" is a palindrome.
	"race a car" is not a palindrome.

Return 0 / 1 ( 0 for false, 1 for true ) for this problem
*/

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * @input A : String termination by '\0'
 * 
 * @Output Integer
 */
int
isPalindrome(char* A)
{
	int i, j;

	i = 0;
	j = strlen(A)-1;

	while (i < j) {
		if (! isalnum(A[i])) {
			i += 1;
			continue;
		}

		if (! isalnum(A[j])) {
			j -= 1;
			continue;
		}

		if (tolower(A[i]) != tolower(A[j]))
			return 0;

		i += 1;
		j -= 1;
	}

	return 1;
}

int
main(void)
{
	assert(isPalindrome("A man, a plan, a canal: Panama") == 1);
	assert(isPalindrome("race a car") == 0);
	return 0;
}

