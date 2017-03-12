'''
https://www.interviewbit.com/problems/add-one-to-number
'''

def add_one(numbers, n):
	carry = 1
	for i in xrange(n-1, -1, -1):
		digit_sum = carry + (numbers[i])
		carry = digit_sum / 10
		digit_sum = digit_sum % 10
		numbers[i] = digit_sum

	if carry == 1:
		numbers.insert(0, 1)

	# Remove leading 0s
	i = 0
	while numbers[i] == 0:
		i += 1

	return numbers[i:]

nList = map(int, raw_input().split())
n = nList[0]
print add_one(nList[1:], n)
