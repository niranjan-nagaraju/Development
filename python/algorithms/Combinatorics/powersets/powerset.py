#!/usr/bin/python

# Add the current subset to the powerset list ordered by size
def add_to_powerset(powerset, prefix):
	i = len(prefix)

	if prefix not in powerset[i]:
		powerset[i].append(prefix)

# Generate subsets starting with given prefix
def generate_power_set_helper(powerset, a, prefix, startindex, n):
	if (startindex >= n):
		return 

	prefix.append(a[startindex])
	add_to_powerset(powerset, prefix[:])

	if len(prefix) == n:
		return 

	generate_power_set_helper(powerset, a, prefix[:], startindex+1, n)
	add_to_powerset(powerset, prefix[:])

	for i in range(startindex+2, n):
		generate_power_set_helper(powerset, a, prefix[:], i, n)
		add_to_powerset(powerset, prefix[:])

# Lose the starting elements of the set, one at a time to generate subsets starting from 2,3,... etc.
def generate_power_set(powerset, a, n):
	for i in range(n, 0, -1):
		generate_power_set_helper (powerset, a, [], 0, i) 
		a.pop(0)

def main():
	n = int(raw_input())
	a = range(1, n+1)

	# Initialize an empty 2D list to store subsets ordered by length
	powerset = []
	for i in range(0, n+1):
		powerset.append([])

	generate_power_set(powerset, a, n)

	for i in range(0, n+1):
		print powerset[i]

if __name__ == "__main__":
	main()

