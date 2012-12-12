#include <stdio.h>
#include <string.h>

int cookie;
char buf[12];

int main(void)
{
    printf("%d\n", cookie);
    gets(buf);
    
    if (cookie == 0x41424344)
        printf("Stack overflown\n");

    return 0;
}
