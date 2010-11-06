#!/usr/bin/python

def pairSumInArray (arr, val):
	low = 0
	high = len(arr) - 1

	while (low < high):
		if (arr[low] + arr[low]) == val:
			return (arr[low], arr[low])
		if (arr[high] + arr[high]) == val:
			return (arr[high], arr[high])

		sum = arr[low] + arr[high]
		if sum == val:
			return (arr[low], arr[high])

		if sum < val:
			low += 1
		else:
			high -= 1

	return None

pair = pairSumInArray([1,2,4,8,16], 24)


if pair == None:
	print "No pair"
else:
	print pair
