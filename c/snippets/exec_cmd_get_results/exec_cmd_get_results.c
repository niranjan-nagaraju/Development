/** 
 * Execute a system command and retrieve the output of the command
 * SOURCE: http://stackoverflow.com/questions/478898/how-to-execute-a-command-and-get-output-of-command-within-c
 * Keyword: popen()
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *
execute_cmd (char* cmd) 
{
	FILE* pipe = popen(cmd, "r");
	char *result = (char *)malloc(1024);
	
	if ( !pipe ) return "ERROR";
		char buffer[128];
		if (! result )
			return NULL;

		while ( !feof(pipe) ) {
			if ( fgets(buffer, 128, pipe ) != NULL) {
				strcat (result, buffer);
			}
		}

		pclose(pipe);
	
	return result;
}

int 
main (void)
{
	char *cmd_result = execute_cmd("ls -l");
	printf("Result is \n%s\n", cmd_result);

	return 0;
}
