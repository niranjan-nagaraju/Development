'''
Task: Jump over numbers

You are given a list of non-negative integers and you start at the left-most integer in this list. After that you need to perform the following step:

Given that the number at the position where you are now is P you need to jump P positions to the right in the list. For example, if you are at position 6 and the number at position 6 has the value 3, you need to jump to position 6 + 3 = 9. Repeat this operation until you reach beyond the right-side border of the list.
Your program must return the number of jumps that it needs to perform following this logic. Note that the list may contain the number 0, which mean that you can get stuck at a this position forever. In such cases you must return the number -1.

The length N of the input list will be in the range [1, 1000].

SAMPLE INPUT
3 4 1 2 5 6 9 0 1 2 3 1

SAMPLE OUTPUT
4
Note: In the sample example you start at position 1, where the number is 3. Then you must jump to position 4, where the number is 2. After that you jump to position 6 where the number is 6. This will lead you to position 12, which is the last number in the list and has the value 1. From there you jump 1 position to the right and must stop. This is a total of 4 jumps.
'''
def jump_over_numbers(list):
	jumps = 0
	idx = 0
	while idx < len(list):
		if list[idx] == 0:
			return -1
		idx = idx + list[idx]
		jumps += 1

	return jumps


if __name__ == '__main__':
	assert jump_over_numbers([]) == 0
	assert jump_over_numbers([3,4,1,2,5,6,9,0,1,2,3,1]) == 4
	assert jump_over_numbers([3,4,5,1,2,4,0,1,3,4]) == -1

