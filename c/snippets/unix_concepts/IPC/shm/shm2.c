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

	/** Get shared mem segment */
	if ( (seg_id = shmget(key, 0x6400, S_IRUSR | S_IWUSR)) < 0 ) {
		perror("shmget");
		exit(1);
	}

	/** Attach the memory segment */
	if ( (shm_mem = (char *)shmat(seg_id, 0, 0)) == (char *)-1) {
		perror("shmat");
		exit(2);
	}

	/** Read from shared memory */
	printf("Buffer from shared memory reads: %s\n", shm_mem);	

	sleep(1);

	printf("NULLing shared memory\n");
	
	/** NULL the first byte, so the other process can exit */
	*shm_mem = 0;

	exit(0);
}

