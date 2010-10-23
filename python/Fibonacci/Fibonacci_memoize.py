#!/bin/python

fibonacci_table = [0]*1000
def fibonacci(n):
    if (fibonacci_table[n] != 0):
        return fibonacci_table[n]
    else:
		fibonacci_table[n] = fibonacci_table[n-1] + fibonacci_table[n-2]
		return fibonacci_table[n]    
    
fibonacci_table[1] = fibonacci_table[2] = 1
print fibonacci(100)
