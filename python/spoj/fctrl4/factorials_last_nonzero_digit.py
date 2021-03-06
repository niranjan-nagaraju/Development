'''
http://www.spoj.com/problems/FCTRL4/

Last non-zero digit in n!

Input file:
1
2
3
4
5
6
7
8

Output file:
1
2
6
4
2
2
4
2
'''

# Return last non-zero from a number 'n'
# e.g., 10-> 1, 8000 -> 8
def last_nonzero_digit(n):
    digit = n % 10
    while digit == 0:
        n = n/10
        digit = n % 10

    return digit

def last_nonzero_digit_in_factorial(n):
    product = 1
    while n>1:
        product = last_nonzero_digit(product * last_nonzero_digit(n))
        n = n - 1

    return last_nonzero_digit(product)

def main():
    while True:
        try:
            n = int(input())
            print last_nonzero_digit_in_factorial(n)
        except EOFError:
            exit(0)


if __name__ == "__main__":
	assert last_nonzero_digit_in_factorial(1) == 1
	assert last_nonzero_digit_in_factorial(2) == 2
	assert last_nonzero_digit_in_factorial(3) == 6
	assert last_nonzero_digit_in_factorial(4) == 4
	assert last_nonzero_digit_in_factorial(5) == 2
	assert last_nonzero_digit_in_factorial(6) == 2
	assert last_nonzero_digit_in_factorial(7) == 4
	assert last_nonzero_digit_in_factorial(8) == 2
	main()
           
