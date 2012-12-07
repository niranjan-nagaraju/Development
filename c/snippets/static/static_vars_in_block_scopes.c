#include <stdio.h>

#define test(a, b)\
    ({ \
     static int block; \
     block++; \
     __test(a,b,&block); \
     })


void __test(int a, int b, int *block)
{
    printf("%d %d %p %d\n", a, b, block, *block);
}

int main(void)
{
    int  i = 0;

    // All of this scope will have the same static 'block'
    for (i=0; i<10; i++) {
        test(i, i+1);
    }

    // A new static 'block' here
    for (i=10; i<20; i++) {
        test(i, i+1);
    }

    return 0;
}
