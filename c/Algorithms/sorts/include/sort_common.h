#ifndef __SORT_COMMON__
#define __SORT_COMMON__

#include <stdio.h>
#include <stdlib.h>
#include <common.h>

void selection_sort(int a[], int n);
void bubble_sort(int a[], int n);
void bubble_sortR(int a[], int n);
void insertion_sort(int a[], int n);
void merge_sort(int a[], int n);
void quick_sort(int a[], int n);
void heap_sort(int a[], int n);
void shell_sort(int a[], int n);
void counting_sort(int a[], int n);
void radix_sort(int a[], int n);
void abacus_sort(int a[], int n);

void swap(int *a, int *b);

#endif

