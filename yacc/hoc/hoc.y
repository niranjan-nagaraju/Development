%{
#include <stdio.h>
#include <ctype.h>
#define YYSTYPE double /** yacc stack; set type to double */
%}
%token NUMBER
%left '+' '-'	/** Left associative, same precedence */
%left '*' '/'	/** Left associative, higher precedence */
%%
list:	/** nothing */
	| list '\n'
	| list expr '\n'	{ printf("\t%.8g\n", $2); }
	;
expr:	NUMBER			{ $$ = $1; }
	|	expr '+' expr	{ $$ = $1 + $3; }
	|	expr '-' expr	{ $$ = $1 - $3; }
	|	expr '*' expr	{ $$ = $1 * $3; }
	|	expr '/' expr	{ $$ = $1 / $3; }
	|	'(' expr ')'	{ $$ = $2; }
	;
%%

char *progname;
int lineno = 1;

int main(int argc, char **argv)
{
	progname = argv[0];
	yyparse();
	return 0;
}

int yylex(void)
{
	int c;

	while ( (c=getchar()) == ' ' || c == '\t');

	if ( c == EOF )
		return 0;
	
	if ( c == '.' || isdigit(c) ) { /** number */
		ungetc(c, stdin);
		scanf("%lf", &yylval);
		return NUMBER;
	}
	if ( c == '\n' )
		lineno++;

	return c;
}

int yyerror(char *s)
{
	fprintf(stderr, "%s: %s near line %d\n", progname, s, lineno);
}
