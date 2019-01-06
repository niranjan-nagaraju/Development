'''
Print a pascal triangle on length k

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
....


input:
4

Output:
1  
1 1  
1 2 1  
1 3 3 1  
'''


def next_row(pascal):
	row = []
	i = 0
	for i in range(len(pascal)-1):	
		row.append(pascal[i] + pascal[i+1])

	return [1] + row + [1]


def print_list(lst):
	for x in lst:
		print x,
	print


# print a pascal's triangle of length 'n' (n rows)
def pascals_triangle(n):
	row = [1]
	for i in range(n):
		print_list(row)
		row = next_row(row)


if __name__ == '__main__':
	pascals_triangle(5)

