#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/** Source: Advanced Linux Programming */

int main(void)
{
	int tmp_fd;
	char buf[10];
	int len;
	char temp_filename[] = "./temp_file.XXXXXX";

	tmp_fd = mkstemp(temp_filename);

	/** unlink immediately, so it'll be deleted when fd is closed */
	unlink(temp_filename);

	write(tmp_fd, "AAAA", 4);

	/** Rewind to beginning of the file */
	lseek(tmp_fd, 0, SEEK_SET);

	read(tmp_fd, buf, 10);

	printf("Tmp file %s reads : %s\n", temp_filename, buf);

	close(tmp_fd);

	return 0;
}
