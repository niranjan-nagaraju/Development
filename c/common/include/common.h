#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdio.h>
#include <boolean.h>

/** define function pointer types for common operations */
typedef int (*comparefn) (void *data1, int idx1, void *data2, int idx2); /** Compare function pointer */
typedef void (*printfn) (void *data);	/** Print function pointer */

/** Function pointer for memory allocators and deallocators */
typedef void *(*allocatorfn) (int size);
typedef void (*deallocatorfn) (void *ptr);
typedef void (*reallocatorfn) (void *ptr, int size);

int comparePtrs (void *p, int idx1, void *q, int idx2);
int compareInts (void *p, int idx1, void *q, int idx2);
int compareChars (void *p, int idx1, void *q, int idx2);

boolean compareIntArrays(void *p, int pi, void *q, int qi, int n);

void swapInt(int *p, int *q);
void swapChar(char *p, char *q);
void swapPtr(void **p, void **q);

void printAsInt(void *object);
void printAsUInt(void *object);
void printAsLong(void *object);
void printAsULong(void *object);
void printAsChar(void *object);
void printAsString(void *object);
void printPtr(void *object);

void printNL(void);
void printSPC(void);
void printTAB(void);

#endif

