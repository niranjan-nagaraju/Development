#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdio.h>

int comparePtrs (void *p, void *q);

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

#endif

