#!/usr/local/bin/python3

'''
Test program to test *args and **kwargs unpacking in function calls
'''


def f1(a, b, c):
	return a+b+c


def f2(a):
	return a+10



def f3():
	return 100



def handler(fn, *args, **kwargs):
	return fn(*args, **kwargs)


if __name__ == '__main__':
	assert(handler(f1, 1, 2, 3) ==  6)
	assert(handler(f2, 3) ==  13)
	assert(handler(f3) ==  100)

	t = (2,3,5)
	assert((f1(*t) == 10))

	p = [6,4,3]
	assert(f1(*p) == 13)

	assert(f2(*[1]) == 11)
	assert(f3(*[]) == 100)

	assert(handler(f1, c="3", b="2", a="1") == "123")

	d = {'a': 1, 'c': 4, 'b': 2}
	assert(handler(f1, **d) == 7)
	x = {'a': 1}
	assert(handler(f2, **x) == 11)

