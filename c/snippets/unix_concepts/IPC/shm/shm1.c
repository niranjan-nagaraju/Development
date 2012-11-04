#include <stdio.h>
#include <stdlib.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/types.h>

int main (void)
{
	int seg_id;
	char *shm_mem;
	struct shmid_ds shmbuffer;
	int seg_size;
	key_t key = 1123;

	/** Create shared mem segment */
	if ( (seg_id = shmget(key, 0x6400, IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR)) < 0 ) {
		perror("shmget");
		exit(1);
	}

	/** Attach the memory segment */
	if ( (shm_mem = (char *)shmat(seg_id, 0, 0)) == (char *)-1) {
		perror("shmat");
		exit(2);
	}

	/* Determine the segment’s size. */
	shmctl (seg_id, IPC_STAT, &shmbuffer);
	seg_size = shmbuffer.shm_segsz;
	printf ("segment size: %d\n", seg_size);

	/* Write a string to the shared memory segment. */
	sprintf (shm_mem, "Hello, world.");

	/** Wait until the other process takes our string and make it a null string */
	while (*shm_mem != '\0') {
		sleep(1);
	}

	printf("Other process NULLed shared memory\n");

	exit(0);
}
