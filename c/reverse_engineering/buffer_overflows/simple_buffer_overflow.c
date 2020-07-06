#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

int
main(void)
{
	char *buffer;
	buffer = malloc(sizeof(char)*8);
	uint32_t value = 0;
	strcpy(buffer, "testAAAA");
	printf("%4X\n", value);

	return 0;
}

/*
 * libc in mac os x aborts memcpy after detecting overwrite
(lldb) bt
* thread #1: tid = 0x13d2b8, 0x00007fff8bc72f06 libsystem_kernel.dylib`__pthread_kill + 10, queue = 'com.apple.main-thread', stop reason = signal SIGABRT
  * frame #0: 0x00007fff8bc72f06 libsystem_kernel.dylib`__pthread_kill + 10
    frame #1: 0x00007fff9d3be4ec libsystem_pthread.dylib`pthread_kill + 90
    frame #2: 0x00007fff98e7e6df libsystem_c.dylib`abort + 129
    frame #3: 0x00007fff98e7e856 libsystem_c.dylib`abort_report_np + 181
    frame #4: 0x00007fff98ea4a0c libsystem_c.dylib`__chk_fail + 48
    frame #5: 0x00007fff98ea49dc libsystem_c.dylib`__chk_fail_overflow + 16
    frame #6: 0x00007fff98ea4f22 libsystem_c.dylib`__memcpy_chk + 37
    frame #7: 0x0000000100000f52 simple_buffer_overflow`main + 34 at simple_buffer_overflow.c:10 [opt]
    frame #8: 0x00007fff8ff725ad libdyld.dylib`start + 1 
*/

/*
Linux:
$ ./out/linux/simple_buffer_overflow 
*** buffer overflow detected ***: ./out/linux/simple_buffer_overflow terminated
 */
