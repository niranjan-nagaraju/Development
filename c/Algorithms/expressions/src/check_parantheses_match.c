#include<array_stack.h>
#include<assert.h>

/**
 * Check if parantheses in an expression are properly nested
 * e.g. 
 *   ((A + B) : False
 *   (A + (B + C)): True
 *
 *   {(A+B)} + [C+D]: true
 *   {A(} + B): False
 */

#define MAX_EXPR_SIZE 100

static boolean
is_matching_parantheses(char s, char e)
{
	switch (e) {
		case ')': 
			return s == '(';
		case ']': 
			return s == '[';
		case '}': 
			return s == '{';
		default: 
			return FALSE;
	}
}


static boolean
is_opening_parantheses(const char s)
{
	return (s == '(' || s == '[' || s == '{');
}

static boolean
is_closing_parantheses(const char s)
{
	return (s == ')' || s == ']' || s == '}');
}

boolean
check_parantheses_match(const char *expr)
{
	int i = 0;
	array_stack_t pstack;
	boolean valid = TRUE;

	array_stack_init(&pstack, MAX_EXPR_SIZE);

	for (i=0; expr[i]; i++) {
		if (is_opening_parantheses(expr[i])) {
			array_stack_push(&pstack, expr[i]);
		} else if (is_closing_parantheses(expr[i])) {
			if (array_stack_isEmpty(&pstack)) {
				valid = FALSE;
				break;
			}
			int symb = array_stack_pop(&pstack);

			/** 'symb' must be matching opener for expr[i] */
			if (! is_matching_parantheses(symb, expr[i]) ) {
				valid = FALSE;
				break;
			}
		}
	}

	if (!array_stack_isEmpty(&pstack)) {
		valid = FALSE;
	}

	array_stack_destroy(&pstack);

	return valid;
}

int
main(void) 
{
	assert(check_parantheses_match("(A+B)")== TRUE);
	assert(check_parantheses_match("{(A+B)}") == TRUE);
	assert(check_parantheses_match("{[B](A+B)}") == TRUE);
	assert(check_parantheses_match("({A+B}[)") == FALSE);
	assert(check_parantheses_match("7 * ((X* ((X+Y)/(J-3))+Y)/(4-2.5))") == TRUE);

	return 0;
}
