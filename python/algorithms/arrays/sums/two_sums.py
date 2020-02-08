'''
Find and return all pairs that add upto a specified target sum
'''

# Return all pairs with sum in a sorted array
def two_sums_sorted(a, target):
	pairs = []
	i, j = 0, len(a)-1
	while i < j:
		curr_sum = a[i]+a[j]
		if curr_sum == target:
			pairs.append((a[i], a[j]))
			i += 1
			j -= 1
		elif curr_sum < target:
			# Move left pointer so current sum increases
			i += 1
		else: # curr_sum > target:
			# Move right pointer so current sum decreases
			j -= 1

	return pairs


if __name__ == '__main__':
	assert two_sums_sorted([1,2,3,4,5,6,7], 8) == [(1,7), (2,6), (3,5)]
	assert two_sums_sorted([1,2,3,4,5], 7) == [(2,5), (3,4)]
