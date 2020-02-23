#!/usr/bin/python

def sum_of_divisors(num):
	#return sum ([x for x in range(2, (num/2)+1) if (num % x == 0)]) + 1
	sum = 1
	i = 2
	while ( i <= (num/2) ):
		if (num % i == 0):
			sum = sum + i
		i = i + 1
	return sum

def get_abundant_numbers ():
	i = 1
	abundant_numbers = []
	while ( i <= 28123 ):
		if (sum_of_divisors(i) > i):
			abundant_numbers.append(i)
		i += 1
	return abundant_numbers

def pairSumInArray (arr, val):
	low = 0
	high = len(arr) - 1

	while (low < high):
		if ((arr[low] + arr[low]) == val):
			return True
		 
		if ((arr[high] + arr[high]) == val):
			return True

		sum = arr[low] + arr[high]
		if sum == val:
			return True
		if sum < val:
			low += 1
		else:
			high -= 1

	return False

def get_numbers_not_sum_ab ():
	ab = get_abundant_numbers()
	count = 0
	i = 1
	while ( i <= 28123 ):
		if (pairSumInArray(ab, i) == False):
			count += i
		i += 1
	return count

print get_numbers_not_sum_ab()
# 4179871
