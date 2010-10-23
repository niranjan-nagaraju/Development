#ifndef __SORT_COMMON__
#define __SORT_COMMON__

#include <stdio.h>
#include <stdlib.h>
#include <common.h>

void selection_sort(int a[], int n);
void bubble_sort(int a[], int n);
void insertion_sort(int a[], int n);
void merge_sort(int a[], int n);
void quick_sort(int a[], int n);

void swap( int *a, int *b);
void getArray(int **a, int n);
void printArray(int *a, int n);

#endif

