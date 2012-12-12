#include <stdio.h>
#include <string.h>

int cookie;
char buf[12];

int main(void)
{
    gets(buf);
    // AAAAAAAAAAAADCBA should cause the print
    
    if (cookie == 0x41424344)
        printf("Stack overflown\n");

    return 0;
}
