#encoding: utf-8
'''
https://www.interviewbit.com/problems/fizzbuzz/

FizzBuzz


Fizzbuzz is one of the most basic problems in the coding interview world. Try to write a small and elegant code for this problem. 
Given a positive integer A, return an array of strings with all the integers from 1 to N.
But for multiples of 3 the array should have “Fizz” instead of the number.
For the multiples of 5, the array should have “Buzz” instead of the number.
For numbers which are multiple of 3 and 5 both, the array should have “FizzBuzz” instead of the number.

Look at the example for more details.

Example

A = 5
Return: [1 2 Fizz 4 Buzz]
'''
class Solution:
	# @param A : integer
	# @return a list of strings
	def fizzBuzz(self, A):
		res = range(1, A+1)
		for i in xrange(A):
			toreplace = ""
			if res[i] % 3 == 0:
				toreplace = "Fizz"
			if res[i] % 5 == 0:
				toreplace = toreplace + "Buzz"

			if toreplace != "":
				res[i] = toreplace

		return res


if __name__ == '__main__':
	s = Solution()
	assert s.fizzBuzz(5) == [1, 2, 'Fizz', 4, 'Buzz']
	assert s.fizzBuzz(16) == [1, 2, 'Fizz', 4, 'Buzz', 'Fizz', 7, 8, 'Fizz', 'Buzz', 11, 'Fizz', 13, 14, 'FizzBuzz', 16]

