/**
https://www.hackerrank.com/challenges/palindrome-index

Given a string, S, of lowercase letters, determine the index of the character whose removal will make S a palindrome. If S is already a palindrome or no such character exists, then print -1. There will always be a valid solution, and any correct answer is acceptable. For example, if S="bcbc", we can either remove 'b' at index 0 or 'c' at index 3.

Output Format
Print an integer denoting the zero-indexed position of the character that makes S not a palindrome; if S is already a palindrome or no such character exists, print -1.

Sample Input
3
aaab
baa
aaa

Sample Output
3
0
-1

*/

#include<stdio.h>
#include<string.h>


/** Check if s is a palindrome without s[idx] */
int
is_palindrome(char *s, int idx, int n)
{
	int i=0, j=n-1;

	while(1) {
		/** skip i,j if either of them == idx */
		if ( i == idx )
			i += 1;
		if ( j == idx )
			j -= 1;

		if (i >= j)
			break;

		if (s[i] != s[j])
			return 1;

		i += 1;
		j -= 1;
	}

	return 0;
}

/** remove 1 char at a time from S and check if its a palindrome */
int
remove_char_and_test(char *s, int n)
{
	int i;
	/** check if the whole string is a palindrome already */
	if (is_palindrome(s, -1, n) == 0)
		return -1;

	for (i=0; i<n; i++) {
		if (is_palindrome(s, i, n) == 0)
			return i;
	}

	return -1;
}

int
main(void)
{
	int t;

	scanf("%d", &t);

	while (t--)
	{
		char s[100005];
		int n;

		scanf("%s", s);
		n = strlen(s);
		
		printf("%d\n", remove_char_and_test(s, n));
	}
}

