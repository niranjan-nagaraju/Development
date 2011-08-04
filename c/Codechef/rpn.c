#include <stdio.h>
#include <malloc.h>
#include <ctype.h>

char *stack[400];
int tos = -1;

void stack_push_expr(char *expr)
{
	stack[++tos] = expr;
}

void stack_push_char(char c)
{
	char *tmp = malloc(2);
	*tmp = c;
	*(tmp +1 ) = 0;

	stack_push_expr(tmp);
}


char *stack_pop(void)
{
	if (tos == -1)
		return NULL;

	return stack[tos--];
}

char *eval_expr (char *left, char *right,  char* operator)
{
	int len1 = strlen(left);
	int len2 = strlen(right);
	char *newstr = malloc(len1+len2+1);

	memcpy(newstr, left, len1); /** skip '\0' of str1 */
	memcpy((newstr+len1), right, len2);
	memcpy((newstr+len1+len2), operator, 2);

	free(left);
	free(right);
	free(operator);

	return newstr;
}

int isOperator(char c)
{
	return (c == '+' || c == '-' || c == '*' || c == '/' || c == '%' || c == '^');
}

void convert_to_rpn(char *expression, char *rpn)
{
	char *right_operand;
	char *left_operand;
	char *operator;
	char *new_entry;

	while (*expression) {
		if (*expression == '(' || isOperator(*expression) || isalpha(*expression))
			stack_push_char(*expression);

		if (*expression == ')') {

			right_operand = stack_pop();
			operator = stack_pop();
			left_operand = stack_pop();
			free(stack_pop()); /** Discard the '(' */

			new_entry = eval_expr(left_operand, right_operand, operator);
			stack_push_expr(new_entry);
		}
		expression++;
	}

	new_entry = stack_pop();
	memcpy(rpn, new_entry, strlen(new_entry));
}


int main(void)
{
	char expression[100][400];
	char rpn[100][400];
	int i, num_expressions;

	scanf("%d", &num_expressions);

	for (i=0; i<num_expressions; i++) {
		scanf("%s", expression[i]);
		convert_to_rpn(expression[i], rpn[i]);
	}


	for (i=0; i<num_expressions; i++) {
		printf("%s\n", rpn[i]);
	}

	return 0;
}


