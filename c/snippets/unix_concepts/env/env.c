#include <stdio.h>
#include <stdlib.h>


/** Source: Advanced Linux Programming */

extern char **environ; /** this contains the environment variables */

int main(void)
{
	char **var;
	char *env_var;

	for (var=environ; *var; var++) {
		printf("%s\n", *var);
	}

	/** Get a specific environment variable */
	printf("\nPATH is %s\n", getenv("PATH"));

	putenv("MYVAR=hello");
	printf("\nMYVAR is %s\n", getenv("MYVAR"));

	return 0;
}
