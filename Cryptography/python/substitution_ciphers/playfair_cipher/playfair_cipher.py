#!/usr/bin/python

original_key = "monarchyyy"
alphabets = range(ord('a'), ord('z')+1)

# Eliminate duplicate characters in key
key = [] 
[key.append(k) for k in original_key if k not in key]
print key

key_matrix = [k.upper() for k in key ] + [chr(k).upper() for k in alphabets if (chr(k) not in key) and (chr(k).upper() != 'I') ]

print key_matrix
